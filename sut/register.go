package sut

import (
	"encoding/json"
	"fmt"
	"log"
	"strconv"
	"strings"
	"time"

	"github.com/symbiont-io/detsys/lib"
)

type SessionId struct {
	Id int
}

func (sessionId SessionId) MarshalJSON() ([]byte, error) {
	return []byte(`"` + strconv.Itoa(sessionId.Id) + `"`), nil
}

func (sessionId *SessionId) UnmarshalJSON(body []byte) (err error) {
	var s string

	if err := json.Unmarshal(body, &s); err != nil {
		return err
	}

	inner, err := strconv.Atoi(s)
	if err != nil {
		return err
	}

	sessionId.Id = inner
	return nil
}

type Register struct {
	value []int
}

func NewRegister() *Register {
	return &Register{
		value: []int{},
	}
}

type Read struct {
}

func (_ Read) Request() {}

type Write struct {
	Value int `json:"value"`
}

func (_ Write) Request() {}

type Value struct {
	Value []int `json:"value"`
}

func (_ Value) Response() {}

type Ack struct {
}

func (_ Ack) Response() {}

type InternalRequest struct {
	Id      SessionId   `json:"id"`
	Request lib.Request `json:"request"`
}

func (_ InternalRequest) Message() {}

type InternalResponse struct {
	Id       SessionId    `json:"id"`
	Response lib.Response `json:"response"`
}

func (_ InternalResponse) Message() {}

func (r *Register) Receive(_ time.Time, from string, event lib.InEvent) []lib.OutEvent {
	var oevs []lib.OutEvent
	switch ev := event.(type) {
	case *lib.InternalMessage:
		switch msg := (*ev).Message.(type) {
		case InternalRequest:
			switch imsg := msg.Request.(type) {
			case Read:
				oevs = []lib.OutEvent{
					{
						To: from,
						Args: &lib.InternalMessage{InternalResponse{
							Id:       msg.Id,
							Response: Value{r.value},
						}},
					},
				}
			case Write:
				r.value = append(r.value, imsg.Value)
				oevs = []lib.OutEvent{
					{
						To: from,
						Args: &lib.InternalMessage{InternalResponse{
							Id:       msg.Id,
							Response: Ack{},
						}},
					},
				}
			default:
				log.Fatalf("Received unknown message: %#v\n", msg)
				return nil

			}
		default:
			log.Fatalf("Received unknown event: %#v\n", msg)
			return nil
		}
	default:
		log.Fatalf("Received unknown event: %#v\n", ev)
		return nil
	}
	return oevs
}

func (r *Register) Tick(_ time.Time) []lib.OutEvent {
	return nil
}

func (_ *Register) ParseRequest(request string, raw json.RawMessage, req *lib.Request) error {
	switch strings.ToLower(request) {
	case "read":
		var op Read
		if err := json.Unmarshal(raw, &op); err != nil {
			panic(err)
		}
		*req = op
	case "write":
		var op Write
		if err := json.Unmarshal(raw, &op); err != nil {
			panic(err)
		}
		*req = op
	default:
		log.Printf("Unknown request type: %s\n%s", request, raw)
	}
	return nil
}

func (_ *Register) ParseResponse(response string, raw json.RawMessage) lib.Response {
	var resp lib.Response
	switch strings.ToLower(response) {
	case "value":
		var op Value
		if err := json.Unmarshal(raw, &op); err != nil {
			panic(err)
		}
		resp = op
	case "ack":
		var op Ack
		if err := json.Unmarshal(raw, &op); err != nil {
			panic(err)
		}
		resp = op
	}
	return resp
}

func (r *Register) ParseMessage(message string, raw json.RawMessage, msg *lib.Message) error {
	switch strings.ToLower(message) {
	case "read":
		var op struct {
			Id      SessionId `json:"id"`
			Request Read      `json:"request"`
		}
		if err := json.Unmarshal(raw, &op); err != nil {
			panic(err)
		}
		*msg = InternalRequest{
			Id:      op.Id,
			Request: op.Request,
		}
	case "write":
		var op struct {
			Id      SessionId `json:"id"`
			Request Write     `json:"request"`
		}
		if err := json.Unmarshal(raw, &op); err != nil {
			panic(err)
		}
		*msg = InternalRequest{
			Id:      op.Id,
			Request: op.Request,
		}
	case "ack":
		var op struct {
			Id       SessionId `json:"id"`
			Response Ack       `json:"response"`
		}
		if err := json.Unmarshal(raw, &op); err != nil {
			panic(err)
		}
		*msg = InternalResponse{
			Id:       op.Id,
			Response: op.Response,
		}
	case "value":
		var op struct {
			Id       SessionId `json:"id"`
			Response Value     `json:"response"`
		}
		if err := json.Unmarshal(raw, &op); err != nil {
			panic(err)
		}
		*msg = InternalResponse{
			Id:       op.Id,
			Response: op.Response,
		}
	default:
		panic(fmt.Errorf("Unknown message type: %s\n%s", message, raw))
	}
	return nil
}

var _ lib.InEvent = lib.ClientRequest{1, Write{2}}
var _ lib.Args = lib.ClientResponse{1, Ack{}}
var _ lib.InEvent = lib.ClientRequest{1, Read{}}
var _ lib.Args = lib.ClientResponse{1, Value{[]int{2}}}

var _ lib.InEvent = lib.InternalMessage{InternalRequest{SessionId{0}, Write{2}}}
var _ lib.Args = lib.InternalMessage{InternalResponse{SessionId{0}, Ack{}}}
var _ lib.InEvent = lib.InternalMessage{InternalRequest{SessionId{0}, Read{}}}
var _ lib.Args = lib.InternalMessage{InternalResponse{SessionId{0}, Value{[]int{2}}}}
var _ lib.Reactor = &Register{}

// ---------------------------------------------------------------------

type FrontEnd struct {
	inFlight                map[uint64]SessionId
	inFlightSessionToClient map[SessionId]uint64
	nextSessionId           int
}

func NewFrontEnd() *FrontEnd {
	return &FrontEnd{map[uint64]SessionId{}, map[SessionId]uint64{}, 0}
}

const register1 string = "register1"
const register2 string = "register2"

func TranslateClientRequestToInternalMessage(req lib.Request, sessionId SessionId) *lib.InternalMessage {
	return &lib.InternalMessage{
		Message: InternalRequest{
			Id:      sessionId,
			Request: req,
		},
	}
}

func (fe *FrontEnd) NewSessionId(clientId uint64) (SessionId, error) {
	var sessionId SessionId
	_, ok := fe.inFlight[clientId]

	if ok {
		return sessionId, fmt.Errorf("Client %d already has a session", clientId)
	}

	sessionId.Id = fe.nextSessionId
	fe.nextSessionId++
	fe.inFlight[clientId] = sessionId
	fe.inFlightSessionToClient[sessionId] = clientId

	return sessionId, nil
}

func (fe *FrontEnd) RemoveSession(sessionId SessionId) (uint64, bool) {
	clientId, ok := fe.inFlightSessionToClient[sessionId]

	if ok {
		delete(fe.inFlight, clientId)
		delete(fe.inFlightSessionToClient, sessionId)
	}

	return clientId, ok
}

func (fe *FrontEnd) ReceiveClient(at time.Time, from string, event lib.ClientRequest) []lib.OutEvent {
	var oevs []lib.OutEvent
	sessionId, err := fe.NewSessionId(event.Id)

	if err != nil {
		// maybe not fatal?
		log.Fatal(err)
		return nil
	}

	oevs = []lib.OutEvent{
		{
			To:   register1,
			Args: TranslateClientRequestToInternalMessage(event.Request, sessionId),
		},
		{
			To:   register2,
			Args: TranslateClientRequestToInternalMessage(event.Request, sessionId),
		},
	}

	// we should also add ack if this was a write, and if so also add an ack..

	return oevs
}

func (fe *FrontEnd) Receive(at time.Time, from string, event lib.InEvent) []lib.OutEvent {
	var oevs []lib.OutEvent
	switch ev := event.(type) {
	case *lib.ClientRequest:
		oevs = fe.ReceiveClient(at, from, *ev)
	case *lib.InternalMessage:
		switch msg := (*ev).Message.(type) {
		case InternalResponse:
			clientId, ok := fe.RemoveSession(msg.Id)
			if ok {
				oevs = []lib.OutEvent{
					{
						To: fmt.Sprintf("client:%d", clientId),
						Args: &lib.ClientResponse{
							Id:       clientId,
							Response: msg.Response,
						},
					},
				}
			}
		default:
			log.Fatalf("Received unknown message: %+v\n", msg)
			return nil
		}
	default:
		log.Fatalf("Received unknown message: %#v\n", ev)
		return nil
	}
	return oevs
}

func (_ *FrontEnd) Tick(_ time.Time) []lib.OutEvent {
	return nil
}

func (n *FrontEnd) ParseRequest(request string, raw json.RawMessage, req *lib.Request) error {
	r := NewRegister()
	return r.ParseRequest(request, raw, req)
}

func (n *FrontEnd) ParseResponse(response string, raw json.RawMessage) lib.Response {
	r := NewRegister()
	return r.ParseResponse(response, raw)
}

func (n *FrontEnd) ParseMessage(message string, raw json.RawMessage, msg *lib.Message) error {
	r := NewRegister()
	return r.ParseMessage(message, raw, msg)
}

func (_ *FrontEnd) MarshalRequest(request lib.Request) string {
	switch ev := request.(type) {
	case Read:
		return "read"
	case Write:
		return "write"
	default:
		panic(fmt.Errorf("Unknown Request event: %#v\n", ev))
	}
}

func (_ *FrontEnd) MarshalResponse(response lib.Response) string {
	switch ev := response.(type) {
	case Ack:
		return "ack"
	case Value:
		return "value"
	default:
		panic(fmt.Errorf("Unknown Response event: %#v\n", ev))
	}
}

func (fe *FrontEnd) MarshalEvent(event lib.Args) string {
	switch ev := event.(type) {
	case *lib.InternalMessage:
		switch msg := (*ev).Message.(type) {
		case InternalRequest:
			return fe.MarshalRequest(msg.Request)
		case InternalResponse:
			return fe.MarshalResponse(msg.Response)
		default:
			panic(fmt.Errorf("Unknown type of InternalMessage: %#v\n", msg))
		}
	case *lib.ClientResponse:
		return fe.MarshalResponse((*ev).Response)

	default:
		panic(fmt.Errorf("Unknown type to marshal: %#v\n", ev))
	}
}
