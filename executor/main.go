package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"time"

	"github.com/symbiont-io/detsys/sut"
)

type ScheduledMessage struct {
	At      time.Time `json:"at"`
	From    string    `json:"from"`
	To      string    `json:"to"`
	Message sut.Rpc   `json:"message"`
}

func handler(topology map[string]sut.Node) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if r.Method != "POST" {
			http.Error(w, "Method is not supported.", http.StatusNotFound)
			return
		}
		var m ScheduledMessage
		err := decodeJSONBody(w, r, &m)
		if err != nil {
			var mr *malformedRequest
			if errors.As(err, &mr) {
				http.Error(w, mr.msg, mr.status)
			} else {
				fmt.Printf("%+v", err)
				http.Error(w, http.StatusText(http.StatusInternalServerError),
					http.StatusInternalServerError)
			}
			return
		}

		if node, ok := topology[m.To]; ok {
			messages := node.Receive(m.At, m.From, m.Message)
			w.Header().Set("Content-Type", "application/json; charset=utf-8")
			j, err := json.Marshal(messages)
			if err != nil {
				panic(err)
			}
			fmt.Fprintf(w, "{\"messages\": %s}", string(j))
		} else {
			fmt.Fprintf(w, "bad receiver `%s'\n", m.To)
			fmt.Fprintf(w, "Message: %+v\n", m)
		}
	}
}

func Deploy(topology map[string]sut.Node) {
	mux := http.NewServeMux()
	mux.HandleFunc("/api/command", handler(topology))
	err := http.ListenAndServe(":3001", mux)
	panic(err)
}

func main() {
	j, _ := json.Marshal(ScheduledMessage{At: time.Now(),
		From:    "node1",
		To:      "node2",
		Message: sut.ClientRequest{Id: 0, Request: sut.Get{}}})
	fmt.Printf("%s\n", string(j))

	const str = `{"at":"2020-09-18T13:19:54.542144085+02:00","from":"node1","to":"node2","message":{"id":0,"request":{"op":"get"}}}`

	var s ScheduledMessage
	err := json.Unmarshal([]byte(str), &s)
	if err != nil {
		panic(err)
	}
	fmt.Printf("%+v\n", s)

	//fmt.Printf("Starting server.\n")
	//Deploy(map[string]sut.Node{"node1": sut.Node{}, "node2": sut.Node{}})
}
