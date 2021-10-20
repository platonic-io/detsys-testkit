package executorEL

import (
	"errors"
	"fmt"
	"os"
	"syscall"
)

func createPipe(fp string) *error {
	fmt.Printf("creating fifo %s\n", fp)
	err := syscall.Mkfifo(fp, 0600)
	if err != nil && !errors.Is(err, os.ErrExist) {
		panic(err)
	}

	return nil
}

func openPipe(fp string, mode int) (*os.File, *error) {

	fmt.Printf("open file %s\n", fp)
	fh, err := os.OpenFile(fp, mode, os.ModeNamedPipe)
	if err != nil {
		fmt.Printf("panic?\n")
		panic(err)
	}
	return fh, nil
}

func writePipe(fp string, data string) {
	err := os.WriteFile(fp, []byte(data), os.ModeNamedPipe)
	if err != nil {
		panic(err)
	}
}
