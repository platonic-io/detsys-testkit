package logger

import (
	"bufio"
	"bytes"
	"context"
	"database/sql"
	"encoding/json"
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
}

func NewLogger() *Logger {
	return &Logger{
		Queue: make(chan []byte, QUEUE_SIZE),
	}
}

type Log struct {
	Event []byte `json:"event"`
	Meta  []byte `json:"meta"`
	Data  []byte `json:"data"`
}

func (_ Log) Message()             {}
func (_ Log) MessageEvent() string { return "log" }

func (l *Logger) Receive(at time.Time, from string, event lib.InEvent) []lib.OutEvent {
	switch ev := event.(type) {
	case *lib.InternalMessage:
		switch msg := (*ev).Message.(type) {
		case Log:
			// XXX: msg.Event and msg.Meta should also be enqueued!
			enqueue(l.Queue,
				bytes.Join([][]byte{msg.Event, msg.Meta, msg.Data},
					[]byte("\t")))
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
	return nil
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
		var op struct {
			Event []byte `json:"event"`
			Meta  []byte `json:"meta"`
			Data  []byte `json:"data"`
		}
		if err := json.Unmarshal(raw, &op); err != nil {
			panic(err)
		}
		*msg = Log{
			Event: op.Event,
			Meta:  op.Meta,
			Data:  op.Data,
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
		// TODO(stevan): If we want or need to support linearisable
		// reads rather than eventual consist ant reads, we could do it
		// as follows. Upon opening the db, save the highest index of
		// the event log table. When reading a line, increment the
		// index, parse the line to determine if it's a write or a read.
		// If it's a write, proceed like below. If it's a read then
		// spawn a new goroutine and pass it the index and the read
		// query. This reader goroutine should then wait until the index
		// is persisted in the db and then perform the read query. This
		// way the reads happen as fast they can while being
		// linearisable and not holding up writes. The efficiency of
		// this solution relies on the assumption that there are many
		// writes between each read.
		line, err := r.ReadBytes('\n')
		for err == nil {
			// TODO(stevan): how can we not only try to parse `log`?
			// The client could prepend the line with the operation
			// and then add a tab to separate it from the data?
			if line != nil {
				log.Printf("ReadBytes: '%s'\n", line)
				var msg lib.Message
				err := m.UnmarshalMessage("log", line, &msg)
				if err != nil {
					panic(err)
				}
				from := "client" // TODO(stevan): make this part of the request?
				oevs := reactor.Receive(time.Now(),
					from,
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

func openPipe(pipeName string) *os.File {
	namedPipe := filepath.Join(os.TempDir(), pipeName)
	syscall.Mkfifo(namedPipe, 0600)

	fh, err := os.OpenFile(namedPipe, os.O_RDONLY, 0600)
	if err != nil {
		panic(err)
	}
	return fh
}
