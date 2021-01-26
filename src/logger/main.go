package logger

import (
	"bufio"
	"bytes"
	"context"
	"database/sql"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"syscall"
	"time"

	_ "github.com/mattn/go-sqlite3"
)

const (
	QUEUE_SIZE int = 1024
	BUFFER_LEN int = 128
)

func main() {
	db := OpenDB()
	fh := OpenPipe()
	queue := make(chan []byte, QUEUE_SIZE)

	go worker(db, queue)

	for {
		scanner := bufio.NewScanner(fh)
		scanner.Split(bufio.ScanLines)
		for scanner.Scan() {
			enqueue(queue, scanner.Bytes())
		}
	}
}

func worker(db *sql.DB, queue chan []byte) {
	buffer := make([][]byte, 0, BUFFER_LEN)
	for {
		if len(buffer) >= BUFFER_LEN {
			commit(db, buffer)
			buffer = buffer[:0]
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
					buffer = buffer[:0]
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
		log.Println("The main thread was blocked for %v due to the queue being full!",
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
	log.Println("The worker thread commited %d entries in %v!", BUFFER_LEN, duration)
}

func parse(entry []byte) ([]byte, []byte, []byte) {
	// NOTE: Tab characters are not allowed to appear unescaped in JSON.
	split := bytes.Split(entry, []byte("\t"))
	if len(split) != 3 {
		panic(fmt.Sprintf("parse: failed to split entry: %v", entry))
	}
	return split[0], split[1], split[2]
}

func OpenDB() *sql.DB {
	path := DBPath()
	// https://stackoverflow.com/questions/35804884/sqlite-concurrent-writing-performance
	db, err := sql.Open("sqlite3",
		path+"?&cache=shared&_journal_mode=WAL&_synchronous=NORMAL")
	if err != nil {
		panic(err)
	}
	db.SetMaxOpenConns(1)
	return db
}

func DBPath() string {
	path, ok := os.LookupEnv("DETSYS_DB")
	if !ok {
		path = os.Getenv("HOME") + "/.detsys.db"
	}
	return path
}

func OpenPipe() *os.File {
	namedPipe := filepath.Join(os.TempDir(), "detsys-logger")
	syscall.Mkfifo(namedPipe, 0600)

	fh, err := os.OpenFile(namedPipe, os.O_RDONLY, 0600)
	if err != nil {
		panic(err)
	}
	return fh
}
