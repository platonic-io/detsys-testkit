package sut

import (
	"encoding/json"
	"fmt"
	"net/http"
	"testing"
	"time"

	"github.com/symbiont-io/detsys/lib"
)

type ScheduledMessage struct {
	at      time.Time
	from    string
	to      string
	message rpc
}

func handler(topology map[string]Node) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if r.Method != "POST" {
			http.Error(w, "Method is not supported.", http.StatusNotFound)
			return
		}
		var m ScheduledMessage
		err := json.NewDecoder(r.Body).Decode(&m)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		node := topology[m.to]
		messages := node.receive(m.at, m.from, m.message)

		w.Header().Set("Content-Type", "application/json; charset=utf-8")
		fmt.Fprintf(w, "Messages: %+v", messages)
	}
}

func Deploy(topology map[string]Node) {
	mux := http.NewServeMux()
	mux.HandleFunc("/api/command", handler(topology))
	err := http.ListenAndServe(":3001", mux)
	panic(err)
}

func DummyTest(t *testing.T) {
	// lib.Deploy(map[string]string{"node1": "Node", "node2": "Node"})
	Deploy(map[string]Node{"node1": Node{}, "node2": Node{}})
	lib.Execute(1)
	// lib.Check
}
