package executorEL

import (
	"encoding/json"
	"fmt"
	// "io/ioutil"
	// "strconv"
	"time"

	// "go.uber.org/zap"
	// "go.uber.org/zap/zapcore"

	"github.com/symbiont-io/detsys-testkit/src/lib"
)

func jsonError(s string) string {
	return fmt.Sprintf("{\"error\":\"%s\"}", s)
}

type StepInfo struct {
	LogLines []string
}

type ComponentUpdate = func(component string) StepInfo

type Executor struct {
	Reactors     []string
	Topology     lib.Topology
	BuildReactor func(name string) lib.Reactor
	Marshaler    lib.Marshaler
	//Update    ComponentUpdate
}

func buildTopology(constructor func(name string) lib.Reactor, reactors []string) lib.Topology {
	items := make([]lib.Item, 0, len(reactors))
	for _, n := range reactors {
		items = append(items, lib.Item{n, constructor(n)})
	}
	return lib.NewTopology(items...)
}

func NewExecutor(reactors []string, buildReactor func(name string) lib.Reactor, m lib.Marshaler) *Executor {
	return &Executor{
		Reactors:     reactors,
		Topology:     buildTopology(buildReactor, reactors),
		BuildReactor: buildReactor,
		Marshaler:    m,
		//Update:    cu,
	}
}

func (ex Executor) Reset() {
	ex.Topology = buildTopology(ex.BuildReactor, ex.Reactors)
}

func (el Executor) processEnvelope(env Envelope) Message {

	msg := env.Message

	var returnMessage json.RawMessage
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
		/* heapDiff := */ jsonDiff(heapBefore, heapAfter)
		// si := el.Update(reactorName)

		returnMessage = lib.MarshalUnscheduledEvents(reactorName, int(env.CorrelationId), oevs)
	case "init":
		var inits = make([]lib.Event, 0)

		reactors := el.Topology.Reactors()
		for _, reactor := range reactors {
			inits = append(inits,
				lib.OutEventsToEvents(reactor, el.Topology.Reactor(reactor).Init())...)
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
			At      time.Time    `json:"at"`
			Meta    lib.MetaInfo `json:"meta"`
		}
		var req TimerRequest
		if err := json.Unmarshal(msg.Message, &req); err != nil {
			panic(err)
		}
		reactor := el.Topology.Reactor(req.Reactor)
		oevs := reactor.Timer(req.At)
		returnMessage = lib.MarshalUnscheduledEvents(req.Reactor, int(env.CorrelationId), oevs)
	case "fault":
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
			el.Topology.Insert(req.Reactor, el.BuildReactor(req.Reactor))
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
	return Message{"what", returnMessage}
}
