package executor

import (
	"encoding/json"
	"errors"
	"fmt"
	"log"
	"net/http"
	"time"

	"github.com/symbiont-io/detsys/lib"
)

type ScheduledMessage struct {
	At      time.Time   `json:"at"`
	From    string      `json:"from"`
	To      string      `json:"to"`
	Command string      `json:"command"`
	Rpc     interface{} `json:"parameters"`
}

type AddressedMessage2 struct {
	From       string      `json:"from"`
	To         string      `json:"to"`
	Command    string      `json:"command"`
	Parameters interface{} `json:"parameters"`
}

func jsonError(s string) string {
	return fmt.Sprintf("{\"error\":\"%s\"}", s)
}

func handler(topology map[string]lib.Reactor) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json; charset=utf-8")
		if r.Method != "POST" {
			http.Error(w, jsonError("Method is not supported."),
				http.StatusNotFound)
			return
		}
		log.Println("Handling request")
		var rpc json.RawMessage
		msg := ScheduledMessage{
			Rpc: &rpc,
		}
		if err := decodeJSONBody(w, r, &msg); err != nil {
			var mr *malformedRequest
			if errors.As(err, &mr) {
				log.Printf("%+v\n", err)
				http.Error(w, jsonError(mr.msg), mr.status)
			} else {
				log.Printf("%+v\n", err)
				http.Error(w,
					jsonError(http.StatusText(
						http.StatusInternalServerError)),
					http.StatusInternalServerError)
			}
			return
		}
		log.Printf("Handling message: %+v", msg)
		if node, ok := topology[msg.To]; ok {
			v := node.Parse(msg.Command, rpc)
			log.Printf("Handling rpc: %+v", v)
			messages := node.Receive(msg.At, msg.From, v)
			var messages2 [2]AddressedMessage2
			for index, message := range messages {
				messages2[index] = AddressedMessage2{
					From:       msg.To,
					To:         message.To,
					Command:    message.Command,
					Parameters: message.Parameters,
				}
			}
			j, err := json.Marshal(messages2)
			if err != nil {
				panic(err)
			}
			fmt.Fprintf(w, "{\"responses\": %s}", string(j))
		} else {
			fmt.Fprintf(w, jsonError(fmt.Sprintf("bad receiver `%s'", msg.To)))
		}
	}
}

func Deploy(topology map[string]lib.Reactor) {
	log.Printf("Deploying topology: %+v\n", topology)
	mux := http.NewServeMux()
	mux.HandleFunc("/api/command", handler(topology))
	err := http.ListenAndServe(":3001", mux)
	panic(err)
}
