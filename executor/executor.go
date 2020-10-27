package executor

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"time"

	"github.com/symbiont-io/detsys/lib"
)

func jsonError(s string) string {
	return fmt.Sprintf("{\"error\":\"%s\"}", s)
}

func handler(topology map[string]lib.Reactor, m lib.Marshaler) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json; charset=utf-8")
		if r.Method != "POST" {
			http.Error(w, jsonError("Method is not supported."),
				http.StatusNotFound)
			return
		}
		r.Body = http.MaxBytesReader(w, r.Body, 1048576)
		// XXX: Inefficient, reuse ideas from:
		// https://www.alexedwards.net/blog/how-to-properly-parse-a-json-request-body ?
		body, err := ioutil.ReadAll(r.Body)
		if err != nil {
			panic(err)
		}
		var sev lib.ScheduledEvent
		if err := lib.UnmarshalScheduledEvent(m, body, &sev); err != nil {
			panic(err)
		}
		log.Printf("Handling event: %+v", sev)
		oevs := topology[sev.To].Receive(sev.At, sev.From, sev.Event)
		bs := lib.MarshalUnscheduledEvents(m, sev.To, oevs)
		fmt.Fprint(w, string(bs))
	}
}

type TickRequest struct {
	Component string `json:"component"`
	At time.Time `json:"at"`
}

func handleTick(topology map[string]lib.Reactor, m lib.Marshaler) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json; charset=utf-8")
		if r.Method != "PUT" {
			http.Error(w, jsonError("Method is not supported."),
				http.StatusNotFound)
			return
		}
		r.Body = http.MaxBytesReader(w, r.Body, 1048576)
		body , err := ioutil.ReadAll(r.Body)
		if err != nil {
			panic(err)
		}
		var req TickRequest
		if err := json.Unmarshal(body, &req); err != nil {
			panic(err)
		}
		oevs := topology[req.Component].Tick(req.At)
		bs := lib.MarshalUnscheduledEvents(m, req.Component, oevs)
		fmt.Fprint(w, string(bs))
	}
}

func Register(topology map[string]lib.Reactor) {
	// TODO(stevan): Make executorUrl part of topology.
	const executorUrl string = "http://localhost:3001/api/v1/event"

	components := make([]string, 0, len(topology))
	for component, _ := range topology {
		components = append(components, component)
	}
	lib.RegisterExecutor(executorUrl, components)
}

func Deploy(srv *http.Server, topology map[string]lib.Reactor, m lib.Marshaler) {
	log.Printf("Deploying topology: %+v\n", topology)
	mux := http.NewServeMux()
	mux.HandleFunc("/api/v1/event", handler(topology, m))
	mux.HandleFunc("/api/v1/tick", handleTick(topology, m))
	srv.Addr = ":3001"
	srv.Handler = mux
	if err := srv.ListenAndServe(); err != http.ErrServerClosed {
		panic(err)
	}
}
