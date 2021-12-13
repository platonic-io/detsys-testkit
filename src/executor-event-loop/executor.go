package executorEL

import (
	"encoding/json"
	"fmt"
	"time"

	"github.com/symbiont-io/detsys-testkit/src/lib"
)

func jsonError(s string) string {
	return fmt.Sprintf("{\"error\":\"%s\"}", s)
}

type ReactorStepInfo struct {
	SimulatedTime time.Time
	LogLines      []string
	StateDiff     json.RawMessage
}

type ReactorsUpdateInfo = map[string]ReactorStepInfo
type ReactorConstructor = func(name string, logBuffer *LogWriter) lib.Reactor
type Buffers = map[string]*LogWriter

type Executor struct {
	Reactors     []string
	Buffers      Buffers
	Topology     lib.Topology
	BuildReactor ReactorConstructor
	Marshaler    lib.Marshaler
}

func buildTopology(constructor ReactorConstructor, reactors []string) (lib.Topology, Buffers) {
	items := make([]lib.Item, 0, len(reactors))
	buffers := make(Buffers)
	for _, n := range reactors {
		buffer := newLogWriter()
		buffers[n] = buffer
		items = append(items, lib.Item{n, constructor(n, buffer)})
	}
	return lib.NewTopology(items...), buffers
}

func NewExecutor(reactors []string, buildReactor ReactorConstructor, m lib.Marshaler) *Executor {
	topo, buffers := buildTopology(buildReactor, reactors)
	e := &Executor{
		Reactors:     reactors,
		Buffers:      buffers,
		Topology:     topo,
		BuildReactor: buildReactor,
		Marshaler:    m,
	}
	return e
}

func (ex Executor) DumpReactorLoglines(name string) []string {
	buffer, ok := ex.Buffers[name]
	if !ok {
		panic(fmt.Sprintf("Couldn't find buffer for %s", name))
	}
	logs := buffer.dump()
	return logs
}

func (ex Executor) Reset() {
	ex.Topology, ex.Buffers = buildTopology(ex.BuildReactor, ex.Reactors)

}

func (el Executor) processEnvelope(env Envelope) (Message, ReactorsUpdateInfo) {

	msg := env.Message

	var returnMessage json.RawMessage
	rui := make(map[string]ReactorStepInfo)
	switch msg.Kind {
	case "message":
		fallthrough
	case "invoke":
		var sev lib.ScheduledEvent
		bytesToDeserialise := msg.Message
		fmt.Printf("About to deserialise message\n%s\n", string(bytesToDeserialise))
		if err := lib.UnmarshalScheduledEvent(el.Marshaler, bytesToDeserialise, &sev); err != nil {
			panic(err)
		}

		reactorName := sev.To // should be from env
		reactor := el.Topology.Reactor(reactorName)
		heapBefore := dumpHeapJson(reactor)
		oevs := reactor.Receive(sev.At, sev.From, sev.Event)
		heapAfter := dumpHeapJson(reactor)
		heapDiff := jsonDiff(heapBefore, heapAfter)
		logLines := el.DumpReactorLoglines(reactorName)

		rui[reactorName] = ReactorStepInfo{
			SimulatedTime: sev.At,
			LogLines:      logLines,
			StateDiff:     heapDiff,
		}

		returnMessage = lib.MarshalUnscheduledEvents(reactorName, int(env.CorrelationId), oevs)
	case "init":
		var inits = make([]lib.Event, 0)

		reactors := el.Topology.Reactors()
		for _, reactorName := range reactors {
			reactor := el.Topology.Reactor(reactorName)
			heapBefore := dumpHeapJson(reactor)
			inits = append(inits, lib.OutEventsToEvents(reactorName, reactor.Init())...)
			heapAfter := dumpHeapJson(reactor)
			heapDiff := jsonDiff(heapBefore, heapAfter)
			logLines := el.DumpReactorLoglines(reactorName)
			rui[reactorName] = ReactorStepInfo{
				SimulatedTime: time.Unix(0, 0).UTC(),
				LogLines:      logLines,
				StateDiff:     heapDiff,
			}

		}

		bs, err := json.Marshal(struct {
			Events        []lib.Event   `json:"events"`
			CorrelationId CorrelationId `json:"corrId"`
		}{inits, env.CorrelationId})
		if err != nil {
			panic(err)
		}
		returnMessage = bs
	case "timer":
		type TimerRequest struct {
			Reactor string       `json:"to"`
			At      lib.TimePico `json:"at"`
			Meta    lib.MetaInfo `json:"meta"`
		}
		var req TimerRequest
		if err := json.Unmarshal(msg.Message, &req); err != nil {
			panic(err)
		}
		reactor := el.Topology.Reactor(req.Reactor)
		heapBefore := dumpHeapJson(reactor)
		oevs := reactor.Timer(time.Time(req.At))
		heapAfter := dumpHeapJson(reactor)
		heapDiff := jsonDiff(heapBefore, heapAfter)
		logLines := el.DumpReactorLoglines(req.Reactor)
		rui[req.Reactor] = ReactorStepInfo{
			SimulatedTime: time.Time(req.At),
			LogLines:      logLines,
			StateDiff:     heapDiff,
		}
		returnMessage = lib.MarshalUnscheduledEvents(req.Reactor, int(env.CorrelationId), oevs)
	case "fault":
		// unclear how to log this as a ReactorsStepInfo...
		type FaultRequest struct {
			Reactor string `json:"to"`
			Event   string `json:"event"`
		}
		var req FaultRequest
		if err := json.Unmarshal(msg.Message, &req); err != nil {
			panic(err)
		}
		switch req.Event {
		case "restart":
			buffer := el.Buffers[req.Reactor]
			el.Topology.Insert(req.Reactor, el.BuildReactor(req.Reactor, buffer))
		default:
			fmt.Printf("Unhandled fault type %s\n", req.Event)
			panic("Unhandled fault type")
		}
		bs, err := json.Marshal(struct {
			Events        []lib.Event   `json:"events"`
			CorrelationId CorrelationId `json:"corrId"`
		}{[]lib.Event{}, env.CorrelationId})
		if err != nil {
			panic(err)
		}
		returnMessage = bs

	default:
		fmt.Printf("Unknown message type: %#v\n", msg.Kind)
		panic("Unknown message type")
	}
	return Message{"Events", returnMessage}, rui
}
