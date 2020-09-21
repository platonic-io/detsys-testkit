package executor

import (
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"time"

	"github.com/symbiont-io/detsys/lib"
)

type ScheduledMessage struct {
	At      time.Time   `json:"at"`
	From    string      `json:"from"`
	To      string      `json:"to"`
	Message string      `json:"message"`
	Rpc     interface{} `json:"parameters"`
}

func handler(topology map[string]lib.Reactor) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if r.Method != "POST" {
			http.Error(w, "Method is not supported.", http.StatusNotFound)
			return
		}
		var rpc json.RawMessage
		msg := ScheduledMessage{
			Rpc: &rpc,
		}
		if err := decodeJSONBody(w, r, &msg); err != nil {
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
		if node, ok := topology[msg.To]; ok {
			v := node.Parse(msg.Message, rpc)
			messages := node.Receive(msg.At, msg.From, v)
			w.Header().Set("Content-Type", "application/json; charset=utf-8")
			j, err := json.Marshal(messages)
			if err != nil {
				panic(err)
			}
			fmt.Fprintf(w, "{\"messages\": %s}", string(j))
		} else {
			fmt.Fprintf(w, "bad receiver `%s'\n", msg.To)
		}
	}
}

func Deploy(topology map[string]lib.Reactor) {
	mux := http.NewServeMux()
	mux.HandleFunc("/api/command", handler(topology))
	err := http.ListenAndServe(":3001", mux)
	panic(err)
}
