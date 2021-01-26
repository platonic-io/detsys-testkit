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
	QUEUE_SIZE       int = 1024
	BUFFER_LEN       int = 128
	MAX_STARVE_COUNT int = 32
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
	var buffer [][]byte
	starve := 0
	for {
		if len(buffer) >= BUFFER_LEN || starve >= MAX_STARVE_COUNT {
			commit(db, buffer)
			buffer = [][]byte{}
		} else {
			item, ok := dequeue(queue)
			if ok {
				buffer = append(buffer, item)
				starve = 0
			} else {
				starve++
			}
		}
	}
}

func enqueue(queue chan []byte, item []byte) {
	var ok bool
	select {
	case queue <- item:
		ok = true
	default:
		ok = false
	}
	if !ok {
		start := time.Now()
		queue <- item
		duration := time.Since(start)
		log.Println("The main thread was blocked for %v due to the queue being full!",
			duration)
	}
}

// Blocking dequeue with timeout.
func dequeue(queue chan []byte) ([]byte, bool) {
	var item []byte
	var ok bool
	select {
	case item = <-queue:
		ok = true
	case <-time.After(200 * time.Millisecond):
		ok = false
	}
	return item, ok
}

func commit(db *sql.DB, buffer [][]byte) {
	start := time.Now()
	ctx := context.Background()
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		panic(err)
	}
	for _, item := range buffer {
		event, meta, data := parse(item)
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
	log.Println("The worker thread commited %d items in %v!", BUFFER_LEN, duration)
}

func parse(item []byte) ([]byte, []byte, []byte) {
	// NOTE: Tab characters are not allowed to appear unescaped in JSON.
	split := bytes.Split(item, []byte("\t"))
	if len(split) != 3 {
		panic(fmt.Sprintf("parse: failed to split item: %v", item))
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
