package lib

import (
	"encoding/json"
	"fmt"
	"log"
	"os/exec"
	"strconv"
	"strings"
	"time"
)

func GenerateTest(test string) TestId {

	cmd := exec.Command("detsys-generator", test)
	out, err := cmd.Output()
	if err != nil {
		log.Fatal(err)
	}

	i, err := strconv.Atoi(strings.TrimRight(string(out), "\n"))

	if err != nil {
		log.Fatal(err)
	}

	return TestId{i}
}

type Topology = map[string]Reactor
type Agenda = []ScheduledEvent

func generateNewEmptyTest() TestId {
	var testId TestId

	{
		db := OpenDB()
		defer db.Close()
		stmt, err := db.Prepare(`INSERT INTO test DEFAULT VALUES`)
		defer stmt.Close()

		_, err = stmt.Exec()
		if err != nil {
			panic(err)
		}
	}

	{
		db := OpenDB()
		defer db.Close()
		stmt, err := db.Prepare(`SELECT max(id) FROM test`)
		defer stmt.Close()

		if err = stmt.QueryRow().Scan(&testId.TestId); err != nil {
			panic(err)
		}
	}

	return testId
}

func setDeploymentHeap(testId TestId, topology Topology) {
	db := OpenDB()
	defer db.Close()

	stmt, err := db.Prepare(`INSERT INTO deployment(test_id, component, args) VALUES(?,?,?)`)
	if err != nil {
		panic(err)
	}
	defer stmt.Close()

	for k, r := range topology {
		j, err := json.Marshal(r)
		if err != nil {
			panic(err)
		}
		_, err = stmt.Exec(testId.TestId, k, j)
		if err != nil {
			panic(err)
		}
	}
}

func setAgenda(testId TestId, agenda Agenda) {
	type Entry struct {
		Kind  string
		Event string
		Args  []byte
		From  string
		To    string
		At    time.Time
	}
	entries := make([]Entry, len(agenda))

	for id, entry := range agenda {
		var kind, event string
		var args []byte

		switch ev := entry.Event.(type) {
		case ClientRequest:
			kind = "invoke"
			event = ev.Request.RequestEvent()
			bs, err := json.Marshal(ev.Request)
			if err != nil {
				panic(err)
			}
			args = bs
		case InternalMessage:
			kind = "message"
			event = ev.Message.MessageEvent()
			bs, err := json.Marshal(ev.Message)
			if err != nil {
				panic(err)
			}
			args = bs
		default:
			panic(fmt.Sprintf("Unknown message type %#v\n", ev))
		}
		entries[id] = Entry{
			Kind:  kind,
			Event: event,
			Args:  args,
			From:  entry.From,
			To:    entry.To,
			At:    entry.At,
		}
	}

	db := OpenDB()
	defer db.Close()

	stmt, err := db.Prepare("INSERT INTO agenda(test_id, id, kind, event, args, `from`, `to`, at) VALUES(?,?,?,?,?,?,?,?)")
	if err != nil {
		panic(err)
	}
	defer stmt.Close()

	for id, entry := range entries {
		_, err = stmt.Exec(testId.TestId, id, entry.Kind, entry.Event, entry.Args, entry.From, entry.To, entry.At.Format(time.RFC3339Nano))
		if err != nil {
			panic(err)
		}
	}
}

func GenerateTestFromTopologyAndAgenda(topology Topology, agenda Agenda) TestId {
	testId := generateNewEmptyTest()
	setDeploymentHeap(testId, topology)
	setAgenda(testId, agenda)

	return testId
}
