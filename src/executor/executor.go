package executor

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"time"

	"go.uber.org/zap"
	"go.uber.org/zap/zapcore"

	"github.com/symbiont-io/detsys/lib"
)

func jsonError(s string) string {
	return fmt.Sprintf("{\"error\":\"%s\"}", s)
}

type ComponentUpdate = func(component string, at time.Time)
type Topology = map[string]lib.Reactor

func handler(db *sql.DB, testId lib.TestId, topology Topology, m lib.Marshaler, cu ComponentUpdate) http.HandlerFunc {
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
		cu(sev.To, sev.At)
		heapBefore := dumpHeapJson(topology[sev.To])
		oevs := topology[sev.To].Receive(sev.At, sev.From, sev.Event)
		heapAfter := dumpHeapJson(topology[sev.To])
		heapDiff := jsonDiff(heapBefore, heapAfter)
		appendHeapTrace(db, testId, sev.To, heapDiff, sev.At)
		bs := lib.MarshalUnscheduledEvents(sev.To, oevs)
		fmt.Fprint(w, string(bs))
	}
}

func handleTick(topology Topology, m lib.Marshaler, cu ComponentUpdate) http.HandlerFunc {
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
		cu(req.Component, req.At)
		oevs := topology[req.Component].Tick(req.At)
		bs := lib.MarshalUnscheduledEvents(req.Component, oevs)
		fmt.Fprint(w, string(bs))
	}
}

func handleTimer(db *sql.DB, testId lib.TestId, topology Topology, m lib.Marshaler, cu ComponentUpdate) http.HandlerFunc {
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
		cu(req.Component, req.At)
		heapBefore := dumpHeapJson(topology[req.Component])
		oevs := topology[req.Component].Timer(req.At)
		heapAfter := dumpHeapJson(topology[req.Component])
		heapDiff := jsonDiff(heapBefore, heapAfter)

		appendHeapTrace(db, testId, req.Component, heapDiff, req.At)
		bs := lib.MarshalUnscheduledEvents(req.Component, oevs)
		fmt.Fprint(w, string(bs))
	}
}

func Register(topology Topology) {
	// TODO(stevan): Make executorUrl part of topology.
	const executorUrl string = "http://localhost:3001/api/v1/"

	components := make([]string, 0, len(topology))
	for component, _ := range topology {
		components = append(components, component)
	}
	lib.RegisterExecutor(executorUrl, components)
}

func DeployWithComponentUpdate(srv *http.Server, testId lib.TestId, topology Topology, m lib.Marshaler, cu ComponentUpdate) {
	mux := http.NewServeMux()

	db := lib.OpenDB()
	mux.HandleFunc("/api/v1/event", handler(db, testId, topology, m, cu))
	mux.HandleFunc("/api/v1/tick", handleTick(topology, m, cu))
	mux.HandleFunc("/api/v1/timer", handleTimer(db, testId, topology, m, cu))
	srv.Addr = ":3001"
	srv.Handler = mux
	if err := srv.ListenAndServe(); err != http.ErrServerClosed {
		panic(err)
	}
	defer db.Close()
}

func Deploy(srv *http.Server, testId lib.TestId, topology Topology, m lib.Marshaler) {
	DeployWithComponentUpdate(srv, testId, topology, m, func(string, time.Time) {})
}

func DeployRaw(srv *http.Server, testId lib.TestId, topology map[string]string, m lib.Marshaler, constructor func(string) lib.Reactor) {
	topologyCooked := make(Topology, len(topology))
	for name, component := range topology {
		topologyCooked[name] = constructor(component)
	}

	Deploy(srv, testId, topologyCooked, m)
}

type LogWriter struct {
	testId    lib.TestId
	runId     lib.RunId
	component string
	at        time.Time
}

func (_ *LogWriter) Sync() error {
	return nil
}

func (lw *LogWriter) Write(p []byte) (n int, err error) {
	if len(p) > 0 {
		// we remove last byte since it is an newline
		lib.AddLogStamp(lw.testId, lw.runId, lw.component, p[:len(p)-1], lw.at)
	}
	return len(p), nil
}

func (lw *LogWriter) AppendToLogger(logger *zap.Logger) *zap.Logger {
	return logger.WithOptions(zap.WrapCore(func(c zapcore.Core) zapcore.Core {
		config := zap.NewDevelopmentEncoderConfig()
		config.TimeKey = ""
		config.LineEnding = ""
		dbLogger := zapcore.NewCore(zapcore.NewConsoleEncoder(config), lw, c)
		return zapcore.NewTee(c, dbLogger)
	}))
}

type Executor struct {
	topology    Topology
	buffers     map[string]*LogWriter
	marshaler   lib.Marshaler
	testId      lib.TestId
	runId       lib.RunId
	constructor func(name string, logger *zap.Logger) lib.Reactor
	logger      *zap.Logger
}

func (e *Executor) ReactorTopology() Topology {
	return e.topology
}

func (e *Executor) SetTestId(testId lib.TestId) {
	e.testId = testId
	for _, buffer := range e.buffers {
		buffer.testId = testId
	}
}

func NewExecutor(testId lib.TestId, marshaler lib.Marshaler, logger *zap.Logger, components []string, constructor func(name string, logger *zap.Logger) lib.Reactor) *Executor {

	runId := lib.RunId{0}

	topology := make(map[string]lib.Reactor)
	buffers := make(map[string]*LogWriter)

	for _, component := range components {
		buffer := &LogWriter{
			testId:    testId,
			runId:     runId,
			component: component,
			at:        time.Time{},
		}
		topology[component] = constructor(component, buffer.AppendToLogger(logger))
		buffers[component] = buffer
	}

	return &Executor{
		topology:    topology,
		buffers:     buffers,
		marshaler:   marshaler,
		testId:      testId,
		runId:       runId,
		constructor: constructor,
		logger:      logger,
	}
}

func (e *Executor) Deploy(srv *http.Server) {
	DeployWithComponentUpdate(srv, e.testId, e.topology, e.marshaler, func(name string, at time.Time) {
		buffer, ok := e.buffers[name]

		if ok {
			buffer.at = at
		} else {
			panic(fmt.Sprintf("Couldn't find buffer for %s", name))
		}
	})
}

func (e *Executor) Register() {
	Register(e.topology)
}

func (e *Executor) Reset(runId lib.RunId) {
	for c, b := range e.buffers {
		b.runId = runId
		b.at = time.Time{}
		e.topology[c] = e.constructor(c, b.AppendToLogger(e.logger))
	}
}
