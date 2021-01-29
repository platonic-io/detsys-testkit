package lib

import (
	"encoding/json"
	"fmt"
	"log"
	"os/exec"
	"reflect"
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

func GenerateTestFromTopologyAndAgenda(topology Topology, agenda Agenda) TestId {
	db := OpenDB()
	defer db.Close()

	var testId TestId
	{
		stmt, err := db.Prepare(`SELECT IFNULL(max(test_id),-1)+1 FROM test_info`)
		defer stmt.Close()

		if err != nil {
			panic(err)
		}

		if err = stmt.QueryRow().Scan(&testId.TestId); err != nil {
			panic(err)
		}
	}

	meta := struct {
		Component string `json:"component"`
		TestId    int    `json:"test-id"`
	}{
		Component: "generator",
		TestId:    testId.TestId,
	}

	type AgendaItem struct {
		Kind  string          `json:"kind"`
		Event string          `json:"event"`
		Args  json.RawMessage `json:"args"`
		From  string          `json:"from"`
		To    string          `json:"to"`
		At    time.Time       `json:"at"`
	}

	entries := make([]AgendaItem, 0, len(agenda))
	for _, entry := range agenda {
		var kind, event string
		var args json.RawMessage

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
		entries = append(entries, AgendaItem{
			Kind:  kind,
			Event: event,
			Args:  args,
			From:  entry.From,
			To:    entry.To,
			At:    entry.At,
		})
	}

	deployment := make([]DeploymentInfo, 0, len(topology))

	for reactor, r := range topology {
		args, err := json.Marshal(r)
		if err != nil {
			panic(err)
		}

		typ := strings.ToLower(strings.Split(reflect.TypeOf(r).String(), ".")[1])
		deployment = append(deployment, DeploymentInfo{
			Reactor: reactor,
			Type:    typ,
			Args:    args,
		})
	}

	data := struct {
		Agenda     []AgendaItem     `json:"agenda"`
		Deployment []DeploymentInfo `json:"deployment"`
	}{
		Agenda:     entries,
		Deployment: deployment,
	}

	EmitEvent(db, "CreateTest", meta, data)

	return testId
}
