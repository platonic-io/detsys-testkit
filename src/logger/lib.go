package logger

import (
	"bufio"
	"bytes"
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"time"

	"github.com/symbiont-io/detsys-testkit/src/lib"
)

const (
	QUEUE_SIZE int = 1024
	BUFFER_LEN int = 512
	PIPE_BUF   int = 512 // POSIX
)

type Logger struct {
	Queue chan []byte
	Index uint64
}

func NewLogger() *Logger {
	return &Logger{
		Queue: make(chan []byte, QUEUE_SIZE),
		// NOTE: The index will get initialised in `Init()`.
		Index: 0,
	}
}

type Log struct {
	Entry []byte
}

func (_ Log) Message()             {}
func (_ Log) MessageEvent() string { return "log" }

type Index struct {
	Component string
}

func (_ Index) Message()             {}
func (_ Index) MessageEvent() string { return "index" }

func (l *Logger) Receive(at time.Time, from string, event lib.InEvent) []lib.OutEvent {
	switch ev := event.(type) {
	case *lib.InternalMessage:
		switch msg := (*ev).Message.(type) {
		case Log:
			enqueue(l.Queue, msg.Entry)
			l.Index++
		case Index:
			writeIndex(msg.Component, l.Index)
		default:
			panic(fmt.Errorf("Unknown message type: %s\n", msg))
		}
	default:
		panic(fmt.Errorf("Unknown event type: %s\n", ev))
	}
	return nil
}

func (l *Logger) Init() []lib.OutEvent {
	log.Println("Initalising Logger reactor")
	db := OpenDB()
	go worker(db, l.Queue)
	l.Index = getHighestIndex(db)
	return nil
}

func getHighestIndex(db *sql.DB) uint64 {
	row := db.QueryRow("SELECT max(rowid) from event_log")
	var index uint64
	err := row.Scan(&index)
	if err != nil {
		panic(err)
	}
	return index
}

func (_ *Logger) Tick(_ time.Time) []lib.OutEvent {
	return nil
}

func (_ *Logger) Timer(_ time.Time) []lib.OutEvent {
	return nil
}

func newBuffer() [][]byte {
	return make([][]byte, 0, BUFFER_LEN)
}

func worker(db *sql.DB, queue chan []byte) {
	log.Println("Logger worker starting up")
	buffer := newBuffer()
	for {
		if len(buffer) >= BUFFER_LEN {
			commit(db, buffer)
			buffer = newBuffer()
		} else {
			if len(buffer) == 0 {
				entry := <-queue // Blocking.
				buffer = append(buffer, entry)
			} else {
				entry, ok := dequeue(queue)
				if ok {
					buffer = append(buffer, entry)
				} else {
					commit(db, buffer)
					buffer = newBuffer()
				}
			}
		}
	}
}

func enqueue(queue chan []byte, entry []byte) {
	var ok bool
	select {
	case queue <- entry:
		ok = true
	default:
		ok = false
	}
	if !ok {
		start := time.Now()
		queue <- entry
		duration := time.Since(start)
		log.Printf("The main thread was blocked for %v due to the queue being full!\n",
			duration)
	}
}

// Blocking dequeue with timeout.
func dequeue(queue chan []byte) ([]byte, bool) {
	var entry []byte
	var ok bool
	select {
	case entry = <-queue:
		ok = true
	case <-time.After(200 * time.Millisecond):
		ok = false
	}
	return entry, ok
}

func commit(db *sql.DB, buffer [][]byte) {
	start := time.Now()
	ctx := context.Background()
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		panic(err)
	}
	for _, entry := range buffer {
		event, meta, data := parse(entry)
		_, err := tx.ExecContext(ctx,
			`INSERT INTO event_log(event, meta, data) VALUES(?, ?, ?)`,
			event, meta, data)
		if err != nil {
			panic(err)
		}
	}
	if err := tx.Commit(); err != nil {
		panic(err)
	}
	duration := time.Since(start)
	log.Printf("The worker thread committed %d entries in %v!\n", len(buffer), duration)
}

func parse(entry []byte) ([]byte, []byte, []byte) {
	// NOTE: Tab characters are not allowed to appear unescaped in JSON.
	split := bytes.Split(entry, []byte("\t"))
	if len(split) != 3 {
		panic(fmt.Sprintf("parse: failed to split entry: '%s'\n", string(entry)))
	}
	return split[0], split[1], split[2]
}

func parseLine(line []byte) ([]byte, []byte, []byte) {
	// NOTE: Tab characters are not allowed to appear unescaped in JSON.
	split := bytes.SplitN(line, []byte("\t"), 3)
	if len(split) != 3 {
		panic(fmt.Sprintf("parseLine: failed to split line: '%s'\n", string(line)))
	}
	return split[0], split[1], split[2]
}

func parseFrom(meta []byte) []byte {
	// NOTE: The only metadata is the sender for now.
	return meta
}

func OpenDB() *sql.DB {
	path := lib.DBPath()
	// https://stackoverflow.com/questions/35804884/sqlite-concurrent-writing-performance
	db, err := sql.Open("sqlite3",
		path+"?&cache=shared&_journal_mode=WAL&_synchronous=NORMAL")
	if err != nil {
		panic(err)
	}
	db.SetMaxOpenConns(1)
	return db
}

type Marshaler struct{}

func NewMarshaler() *Marshaler {
	return &Marshaler{}
}

func (_ *Marshaler) UnmarshalMessage(message string, raw json.RawMessage, msg *lib.Message) error {
	switch strings.ToLower(message) {
	case "log":
		*msg = Log{
			Entry: raw,
		}
	case "index":
		*msg = Index{
			Component: strings.TrimSpace(string(raw)),
		}
	default:
		panic(fmt.Errorf("Unknown message type: %s\n%s", message, raw))
	}
	return nil
}

func (_ *Marshaler) UnmarshalRequest(request string, raw json.RawMessage, req *lib.Request) error {
	return nil
}

func DeployReadOnlyPipe(pipeName string, reactor lib.Reactor, m lib.Marshaler) {

	log.Println("Deploying reactor on a read-only pipe")

	fh := openPipe(pipeName)
	r := bufio.NewReaderSize(fh, PIPE_BUF)

	for {
		line, err := r.ReadBytes('\n')
		for err == nil {
			if line != nil {
				var msg lib.Message
				log.Printf("line = %s\n", line)
				event, meta, data := parseLine(line)
				err := m.UnmarshalMessage(string(event), data, &msg)
				if err != nil {
					panic(err)
				}
				from := parseFrom(meta)
				oevs := reactor.Receive(time.Now(),
					string(from),
					&lib.InternalMessage{msg})
				if oevs != nil {
					panic("read only pipe")
				}
			}
			line, err = r.ReadBytes('\n')
		}
		if err != io.EOF {
			panic(err)
		} else {
			// Avoid getting into a 100% CPU loop if there's nothing
			// to read from the pipe.
			time.Sleep(50 * time.Millisecond)
		}
	}
}

func writeIndex(component string, index uint64) {
	// NOTE: Opening and closing these pipes is not ideal from a performance
	// perspective, but read requests (which ask for the index) shouldn't be
	// too common, so perhaps it's fine.
	namedPipe := filepath.Join(os.TempDir(), fmt.Sprintf("detsys-%s", component))

	fh, err := os.OpenFile(namedPipe, os.O_WRONLY, 0600)
	defer fh.Close()
	if err != nil {
		panic(err)
	}
	fmt.Fprintf(fh, "%d\n", index)
}

func openPipe(pipeName string) *os.File {
	namedPipe := filepath.Join(os.TempDir(), pipeName)
	err := syscall.Mkfifo(namedPipe, 0600)
	if !errors.Is(err, os.ErrExist) {
		panic(err)
	}

	fh, err := os.OpenFile(namedPipe, os.O_RDONLY, 0600)
	if err != nil {
		panic(err)
	}
	return fh
}
