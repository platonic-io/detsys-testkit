package executor

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"time"

	"github.com/symbiont-io/detsys/lib"
)

func jsonError(s string) string {
	return fmt.Sprintf("{\"error\":\"%s\"}", s)
}

func handler(db *sql.DB, testId lib.TestId, topology map[string]lib.Reactor, m lib.Marshaler) http.HandlerFunc {
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
		heapBefore := dumpHeapJson(topology[sev.To])
		oevs := topology[sev.To].Receive(sev.At, sev.From, sev.Event)
		heapAfter := dumpHeapJson(topology[sev.To])
		heapDiff := jsonDiff(heapBefore, heapAfter)
		appendHeapTrace(db, testId, sev.To, heapDiff, sev.At)
		bs := lib.MarshalUnscheduledEvents(sev.To, oevs)
		fmt.Fprint(w, string(bs))
	}
}

func handleTick(topology map[string]lib.Reactor, m lib.Marshaler) http.HandlerFunc {
	type TickRequest struct {
		Component string    `json:"component"`
		At        time.Time `json:"at"`
	}
	return func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json; charset=utf-8")
		if r.Method != "PUT" {
			http.Error(w, jsonError("Method is not supported."),
				http.StatusNotFound)
			return
		}
		r.Body = http.MaxBytesReader(w, r.Body, 1048576)
		body, err := ioutil.ReadAll(r.Body)
		if err != nil {
			panic(err)
		}
		var req TickRequest
		if err := json.Unmarshal(body, &req); err != nil {
			panic(err)
		}
		oevs := topology[req.Component].Tick(req.At)
		bs := lib.MarshalUnscheduledEvents(req.Component, oevs)
		fmt.Fprint(w, string(bs))
	}
}

func handleTimer(db *sql.DB, testId lib.TestId, topology map[string]lib.Reactor, m lib.Marshaler) http.HandlerFunc {
	type TimerRequest struct {
		Component string    `json:"to"`
		At        time.Time `json:"at"`
	}

	return func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json; charset=utf-8")
		if r.Method != "POST" {
			http.Error(w, jsonError("Method is not supported."),
				http.StatusNotFound)
			return
		}
		r.Body = http.MaxBytesReader(w, r.Body, 1048576)
		body, err := ioutil.ReadAll(r.Body)
		if err != nil {
			panic(err)
		}
		var req TimerRequest
		if err := json.Unmarshal(body, &req); err != nil {
			panic(err)
		}
		heapBefore := dumpHeapJson(topology[req.Component])
		oevs := topology[req.Component].Timer(req.At)
		heapAfter := dumpHeapJson(topology[req.Component])
		heapDiff := jsonDiff(heapBefore, heapAfter)

		appendHeapTrace(db, testId, req.Component, heapDiff, req.At)
		bs := lib.MarshalUnscheduledEvents(req.Component, oevs)
		fmt.Fprint(w, string(bs))
	}
}

func Register(topology map[string]lib.Reactor) {
	// TODO(stevan): Make executorUrl part of topology.
	const executorUrl string = "http://localhost:3001/api/v1/"

	components := make([]string, 0, len(topology))
	for component, _ := range topology {
		components = append(components, component)
	}
	lib.RegisterExecutor(executorUrl, components)
}

func Deploy(srv *http.Server, testId lib.TestId, topology map[string]lib.Reactor, m lib.Marshaler) {
	mux := http.NewServeMux()

	db := lib.OpenDB()
	mux.HandleFunc("/api/v1/event", handler(db, testId, topology, m))
	mux.HandleFunc("/api/v1/tick", handleTick(topology, m))
	mux.HandleFunc("/api/v1/timer", handleTimer(db, testId, topology, m))
	srv.Addr = ":3001"
	srv.Handler = mux
	if err := srv.ListenAndServe(); err != http.ErrServerClosed {
		panic(err)
	}
	defer db.Close()
}

func DeployRaw(srv *http.Server, testId lib.TestId, topology map[string]string, m lib.Marshaler, constructor func(string) lib.Reactor) {
	topologyCooked := make(map[string]lib.Reactor, len(topology))
	for name, component := range topology {
		topologyCooked[name] = constructor(component)
	}

	Deploy(srv, testId, topologyCooked, m)
}
