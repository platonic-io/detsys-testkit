package main

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
)

const PIPE_BUF int = 512

func log(w *bufio.Writer, event []byte, meta []byte, data []byte) {
	entry := bytes.Join([][]byte{event, meta, data}, []byte("\t"))
	outerEvent := append(bytes.Join([][]byte{[]byte("log"), []byte("test_client"), entry}, []byte("\t")),
		byte('\n'))
	fmt.Printf("event = '%s'\n", string(outerEvent))
	_, err := w.Write(outerEvent)
	if err != nil {
		panic(err)
	}
	if err := w.Flush(); err != nil {
		panic(err)
	}
}

func index(w *bufio.Writer) uint64 {
	namedPipe := filepath.Join(os.TempDir(), "detsys-test_client")
	err := syscall.Mkfifo(namedPipe, 0600)
	if err != nil && !errors.Is(err, os.ErrExist) {
		panic(err)
	}
	fh, err := os.OpenFile(namedPipe, os.O_RDWR, 0600)
	if err != nil {
		panic(err)
	}
	defer fh.Close()

	from := []byte("test_client")
	event := append(bytes.Join([][]byte{[]byte("index"), from, from}, []byte("\t")), byte('\n'))
	fmt.Printf("event = '%s'\n", string(event))
	_, err = w.Write(event)
	if err != nil {
		panic(err)
	}
	if err := w.Flush(); err != nil {
		panic(err)
	}

	// Read the response from the logger.

	var ix uint64
	r := bufio.NewReaderSize(fh, PIPE_BUF)
	line, err := r.ReadString('\n')
	fmt.Printf("line = %s\n", line)
	for err == nil {
		if line != "" {
			ix, err = strconv.ParseUint(strings.TrimSpace(line), 10, 64)
			if err != nil {
				panic(err)
			}
			break
		}
		line, err = r.ReadString('\n')
	}
	if err != nil && err != io.EOF {
		panic(err)
	}
	return ix
}

func assert(i, j uint64) {
	if i != j {
		panic(fmt.Sprintf("i != j (%d != %d)", i, j))
	}
}

func main() {
	namedPipe := filepath.Join(os.TempDir(), "detsys-logger")
	fh, err := os.OpenFile(namedPipe, os.O_WRONLY, 0600)
	defer fh.Close()
	if err != nil {
		panic(err)
	}
	w := bufio.NewWriter(fh)

	var last_read_index uint64 = 0

	for i := 0; i < 5000; i++ {
		if i%500 == 0 && i != 0 {
			fmt.Printf("Writing... i = %d\n", i)
			j := index(w)
			if last_read_index != 0 {
				assert(j, last_read_index+500)
			}
			last_read_index = j

		}
		log(w, []byte("TestEvent"),
			[]byte(`{"component":"test_client", "test_id":0, "run_id":0}`),
			[]byte(fmt.Sprintf(`{"args":{"index": %d}}`, i)),
		)
	}
}
