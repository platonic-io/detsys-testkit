package executorEL

import (
	"encoding/json"
	"fmt"
	// "io/ioutil"
	// "strconv"
	// "time"

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
	Topology  lib.Topology
	Marshaler lib.Marshaler
	//Update    ComponentUpdate
}

func NewExecutor(topology lib.Topology, m lib.Marshaler) *Executor {
	return &Executor{
		Topology:  topology,
		Marshaler: m,
		//Update:    cu,
	}
}

func (el Executor) processEnvelope(env Envelope) Message {

	msg := env.Message

	var returnMessage json.RawMessage
	switch msg.Kind {
	case "receive":
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
	default:
		fmt.Printf("Unknown message type: %#v\n", msg.Kind)
		panic("Unknown message type")
	}
	return Message{"what", returnMessage}
}
