package sut

import (
	"encoding/json"
	"fmt"
	"log"
	"time"

	"github.com/symbiont-io/detsys/lib"
)

type Register struct {
	value int
}

func NewRegister() *Register {
	return &Register{
		value: 0,
	}
}

type Read struct{}

func (_ Read) Request() {}
func (_ Read) Message() {}

type Write struct {
	Value int `json:"value"`
}

func (_ Write) Request() {}
func (_ Write) Message() {}

type Value struct {
	Value int `json: "value"`
}

type Ack struct{}

func (_ Value) Response() {}
func (_ Value) Message()  {}
func (_ Ack) Response()   {}
func (_ Ack) Message()    {}

func (r *Register) Receive(_ time.Time, from string, event lib.Event) []lib.OutEvent {
	var oevs []lib.OutEvent
	switch ev := event.(type) {
	case lib.InternalMessage:
		switch msg := ev.Message.(type) {
		case Read:
			oevs = []lib.OutEvent{
				{
					To:         from,
					Event:      "Value",
					Parameters: lib.InternalMessage{Value{r.value}},
				},
			}
		case Write:
			r.value = msg.Value
			oevs = []lib.OutEvent{
				{
					To:         from,
					Event:      "Ack",
					Parameters: lib.InternalMessage{Ack{}},
				},
			}
		default:
			log.Fatalf("Received unknown message: %+v\n", msg)
			return nil
		}
	default:
		log.Fatalf("Received unknown event: %+v\n", ev)
		return nil
	}
	return oevs
}

func (r *Register) Tick(_ time.Time) []lib.OutEvent {
	return nil
}

func (_ *Register) ParseRequest(request string, raw json.RawMessage) lib.Request {
	var req lib.Request
	switch request {
	case "Read":
		var op Read
		if err := json.Unmarshal(raw, &op); err != nil {
			panic(err)
		}
		req = op
	case "Write":
		var op Write
		if err := json.Unmarshal(raw, &op); err != nil {
			panic(err)
		}
		req = op
	}
	return req
}

func (_ *Register) ParseResponse(response string, raw json.RawMessage) lib.Response {
	var resp lib.Response
	switch response {
	case "Value":
		var op Value
		if err := json.Unmarshal(raw, &op); err != nil {
			panic(err)
		}
		resp = op
	case "Ack":
		var op Ack
		if err := json.Unmarshal(raw, &op); err != nil {
			panic(err)
		}
		resp = op
	}
	return resp
}

func (r *Register) ParseMessage(message string, raw json.RawMessage) lib.Message {
	var msg lib.Message
	switch message {
	case "Read":
		var op Read
		if err := json.Unmarshal(raw, &op); err != nil {
			panic(err)
		}
		msg = op
	case "Write":
		var op Write
		if err := json.Unmarshal(raw, &op); err != nil {
			panic(err)
		}
		msg = op
	}
	return msg
}

var _ lib.Event = lib.ClientRequest{1, Write{2}}
var _ lib.Event = lib.ClientResponse{1, Ack{}}
var _ lib.Event = lib.ClientRequest{1, Read{}}
var _ lib.Event = lib.ClientResponse{1, Value{2}}

var _ lib.Event = lib.InternalMessage{Write{2}}
var _ lib.Event = lib.InternalMessage{Ack{}}
var _ lib.Event = lib.InternalMessage{Read{}}
var _ lib.Event = lib.InternalMessage{Value{2}}
var _ lib.Reactor = &Register{}

// ---------------------------------------------------------------------

type FrontEnd struct {
	inFlight map[uint64]time.Time
}

func NewFrontEnd() *FrontEnd {
	return &FrontEnd{nil}
}

const register1 string = "register1"
const register2 string = "register2"

func (fe *FrontEnd) Receive(at time.Time, from string, event lib.Event) []lib.OutEvent {
	var oevs []lib.OutEvent
	switch ev := event.(type) {
	case lib.ClientRequest:
		fe.inFlight[ev.Id] = at // TODO(stevan): Throw error if key is already set.
		oevs = []lib.OutEvent{
			{
				To:         register1,
				Event:      "ClientRequest",
				Parameters: ev,
			},
			{
				To:         register2,
				Event:      "ClientRequest",
				Parameters: ev,
			},
		}
	case lib.ClientResponse:
		delete(fe.inFlight, ev.Id) // TODO(stevan): Panic if key isn't set.
		oevs = []lib.OutEvent{
			{
				To:         fmt.Sprintf("client:%d", ev.Id),
				Event:      "ClientResponse",
				Parameters: ev,
			},
		}
	default:
		log.Fatalf("Received unknown message: %+v\n", ev)
		return nil
	}
	return oevs
}

func (_ *FrontEnd) Tick(_ time.Time) []lib.OutEvent {
	return nil
}

func (n *FrontEnd) ParseRequest(request string, raw json.RawMessage) lib.Request {
	r := NewRegister()
	return r.ParseRequest(request, raw)
}

func (n *FrontEnd) ParseResponse(response string, raw json.RawMessage) lib.Response {
	r := NewRegister()
	return r.ParseResponse(response, raw)
}

func (n *FrontEnd) ParseMessage(message string, raw json.RawMessage) lib.Message {
	r := NewRegister()
	return r.ParseMessage(message, raw)
}
