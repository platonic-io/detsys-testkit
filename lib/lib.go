package lib

import (
	"encoding/json"
	"fmt"
	"strconv"
	"strings"
	"time"
)

type ScheduledEvent struct {
	At    time.Time `json:"at"`
	From  string    `json:"from"`
	To    string    `json:"to"`
	Event InEvent   `json:"event"`
}

type InEvent interface{ InEvent() }

type ClientRequest struct {
	Id      uint64
	Request Request
}

type Request interface{ Request() }

func (_ ClientRequest) InEvent() {}

type InternalMessage struct {
	Message Message `json:"message"`
}

type Message interface{ Message() }

func (_ InternalMessage) InEvent() {}

// TODO(stevan): type Fault? Only kill node?

type UnscheduledEvent struct {
	To    string `json:"to"`
	From  string `json:"from"`
	Kind  string `json:"kind"`
	Event string `json:"event"`
	Args  Args   `json:"args"`
}

type Args interface{ Args() }

type ClientResponse struct {
	Id       uint64   `json:"id"`
	Response Response `json:"response"`
}

type Response interface{ Response() }

func (_ ClientResponse) Args()  {}
func (_ InternalMessage) Args() {}

type OutEvent struct {
	To   string
	Args Args
}

type Reactor interface {
	Receive(at time.Time, from string, event InEvent) []OutEvent
	Tick(at time.Time) []OutEvent
}

type Unmarshaler interface {
	ParseRequest(request string, input json.RawMessage, output *Request) error
	// ParseResponse(response string, raw json.RawMessage) Response
	ParseMessage(message string, input json.RawMessage, output *Message) error
}

func UnmarshalScheduledEvent(un Unmarshaler, input []byte, output *ScheduledEvent) error {
	var s struct {
		At    time.Time       `json:"at"`
		From  string          `json:"from"`
		To    string          `json:"to"`
		Kind  string          `json:"kind"`
		Event string          `json:"event"`
		Args  json.RawMessage `json:"args"`
	}
	if err := json.Unmarshal(input, &s); err != nil {
		return err
	}
	iev, err := unmarshal(un, s.Kind, s.Event, s.From, s.Args)
	if err != nil {
		return err
	}

	*output = ScheduledEvent{
		At:    s.At,
		From:  s.From,
		To:    s.To,
		Event: iev,
	}
	return nil
}

func parseClientId(from string) (uint64, error) {
	ss := strings.Split(from, ":")
	error := fmt.Errorf("parseClientId: invalid client: %s\n", from)

	if len(ss) != 2 {
		return 0, error
	}

	if ss[0] != "client" {
		return 0, error
	}

	n, err := strconv.ParseUint(ss[1], 10, 64)
	if err != nil {
		return 0, error
	}

	return n, nil
}

func unmarshal(un Unmarshaler, kind string, event string, from string, input json.RawMessage) (InEvent, error) {
	var iev InEvent
	switch kind {
	case "invoke":
		var req Request
		if err := un.ParseRequest(event, input, &req); err != nil {
			return nil, err
		}
		id, err := parseClientId(from)
		if err != nil {
			return nil, err
		}
		iev = &ClientRequest{
			Id:      id,
			Request: req,
		}
	case "message":
		var msg Message
		if err := un.ParseMessage(event, input, &msg); err != nil {
			return nil, err
		}
		iev = &InternalMessage{
			Message: msg,
		}
	case "fault":
		return nil, nil
	default:
		return nil, fmt.Errorf("unmarshal: unknown `kind': %s\n", kind)
	}
	return iev, nil
}

// func parseEvent(r Reactor, kind string, event string, input json.RawMessage) Event {
// 	var ev Event
// 	switch kind {
// 	case "InternalMessage":
// 		var s struct {
// 			Message json.RawMessage `json:"message"`
// 		}
// 		if err := json.Unmarshal(input, &s); err != nil {
// 			panic(err)
// 		}
// 		msg := r.ParseMessage(event, s.Message)
// 		ev = InternalMessage{
// 			Message: msg,
// 		}
// 	default:
// 		panic(kind)
// 	}
// 	return ev
// }

type Marshaler interface {
	MarshalKind(_ Args) string
	MarshalEvent(_ Args) string
}

type Events struct {
	Events []UnscheduledEvent `json:"events"`
}

func MarshalUnscheduledEvents(m Marshaler, from string, oevs []OutEvent) json.RawMessage {
	usevs := make([]UnscheduledEvent, len(oevs))
	for index, oev := range oevs {
		usevs[index] = UnscheduledEvent{
			From:  from,
			To:    oev.To,
			Kind:  m.MarshalKind(oev.Args),
			Event: m.MarshalEvent(oev.Args),
			Args:  oev.Args,
		}
	}
	bs, err := json.Marshal(Events{usevs})
	if err != nil {
		panic(err)
	}
	return bs
}

// ---------------------------------------------------------------------

var quit chan struct{}

func Setup(f func()) {
	quit = make(chan struct{})
	go loop(f)
}

func Teardown() {
	close(quit)
}

func loop(f func()) {
	for {
		select {
		case <-quit:
			return
		default:
			f()
		}
	}
}
