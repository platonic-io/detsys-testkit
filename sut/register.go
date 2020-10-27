package sut

import (
	"fmt"
	"log"
	"time"

	"github.com/symbiont-io/detsys/lib"
)

type Register struct {
	value []int
	sessions []SessionId
}

func NewRegister() *Register {
	return &Register{
		value: []int{},
		sessions: []SessionId{},
	}
}

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
				found := false
				for _, session := range r.sessions {
					if session == msg.Id {
						found = true
						break
					}
				}
				if !found {
					r.value = append(r.value, imsg.Value)
					r.sessions = append(r.sessions, msg.Id)
				}
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

type SessionIdWithContext struct {
	Id SessionId
	At time.Time
}

type OnGoingInternalRequest struct {
	Register      string
	Request       lib.Request
	SessionId     SessionId
	At            time.Time
	NumberOfTries int
}

type FrontEnd struct {
	inFlight                map[uint64]SessionIdWithContext
	inFlightSessionToClient map[SessionId]uint64
	nextSessionId           int
}

type FrontEnd2 struct {
	onGoing       []OnGoingInternalRequest
	inFlight      map[SessionId]uint64
	receivedResponse map[SessionId]bool
	nextSessionId int
}

func NewFrontEnd() *FrontEnd {
	return &FrontEnd{map[uint64]SessionIdWithContext{}, map[SessionId]uint64{}, 0}
}

func NewFrontEnd2() *FrontEnd2 {
	return &FrontEnd2{[]OnGoingInternalRequest{}, map[SessionId]uint64{}, map[SessionId]bool{}, 0}
}


const register1 string = "register1"
const register2 string = "register2"

func (fe *FrontEnd) NewSessionId(clientId uint64, at time.Time) (SessionId, error) {
	var sessionId SessionId
	_, ok := fe.inFlight[clientId]

	if ok {
		return sessionId, fmt.Errorf("Client %d already has a session", clientId)
	}

	sessionId.Id = fe.nextSessionId
	fe.nextSessionId++
	fe.inFlight[clientId] = SessionIdWithContext{
		Id: sessionId,
		At: at,
	}
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

func translate(req lib.Request, sessionId SessionId) *lib.InternalMessage {
	return &lib.InternalMessage{
		Message: InternalRequest{
			Id:      sessionId,
			Request: req,
		},
	}
}

func (fe *FrontEnd) ReceiveClient(at time.Time, from string, event lib.ClientRequest) []lib.OutEvent {
	var oevs []lib.OutEvent
	sessionId, err := fe.NewSessionId(event.Id, at)

	if err != nil {
		// maybe not fatal?
		log.Fatal(err)
		return nil
	}

	oevs = []lib.OutEvent{
		{
			To:   register1,
			Args: translate(event.Request, sessionId),
		},
		{
			To:   register2,
			Args: translate(event.Request, sessionId),
		},
	}

	return oevs
}

func (fe *FrontEnd2) ReceiveClient(at time.Time, from string, event lib.ClientRequest) []lib.OutEvent {
	sessionId := SessionId{fe.nextSessionId}
	fe.nextSessionId++

	fe.inFlight[sessionId] = event.Id
	fe.receivedResponse[sessionId] = false

	args := translate(event.Request, sessionId)

	fe.onGoing = append(fe.onGoing, OnGoingInternalRequest{
		Register: register1,
		Request: event.Request,
		SessionId: sessionId,
		At: at,
		NumberOfTries: 0,
	})
	fe.onGoing = append(fe.onGoing, OnGoingInternalRequest{
		Register: register2,
		Request: event.Request,
		SessionId: sessionId,
		At: at,
		NumberOfTries: 0,
	})

	return []lib.OutEvent{
		{
			To: register1,
			Args: args,
		},
		{
			To: register2,
			Args: args,
		},
	}

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

func (fe *FrontEnd2) RemoveSession(sessionId SessionId) (uint64, bool) {
	clientId, ok := fe.inFlight[sessionId]

	if ok {
		delete(fe.inFlight, sessionId)

		temp := fe.onGoing[:0]

		for _, on := range fe.onGoing {
			if on.SessionId != sessionId {
				temp = append(temp, on)
			}
		}

		fe.onGoing = temp
	}

	return clientId, ok
}

func (fe *FrontEnd2) Receive(at time.Time, from string, event lib.InEvent) []lib.OutEvent {
	var oevs []lib.OutEvent
	switch ev := event.(type) {
	case *lib.ClientRequest:
		oevs = fe.ReceiveClient(at, from, *ev)
	case *lib.InternalMessage:
		switch msg := (*ev).Message.(type) {
		case InternalResponse:
			if new, ok := fe.receivedResponse[msg.Id]; !new || !ok {
				fe.receivedResponse[msg.Id] = true
				break
			}
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

func (fe *FrontEnd) Tick(at time.Time) []lib.OutEvent {
	duration, _ := time.ParseDuration("25s")
	for key, value := range fe.inFlight {
		if (at.After(value.At.Add(duration))) {
			delete(fe.inFlight, key)
		}
	}

	return nil
}

func (fe *FrontEnd2) Tick(at time.Time) []lib.OutEvent {
	resend := []lib.OutEvent{}

	resendTimer, _ := time.ParseDuration("5s")

	for _, on := range fe.onGoing {
		if (at.After(on.At.Add(resendTimer))) {
			on.At = at
			on.NumberOfTries++
			event := lib.OutEvent{
				To: on.Register,
				Args: translate(on.Request, on.SessionId),
			}
			resend = append(resend, event)
		}
	}

	return resend
}
