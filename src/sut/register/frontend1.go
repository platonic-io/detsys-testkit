package sut

import (
	"fmt"
	"log"
	"time"

	"github.com/symbiont-io/detsys-testkit/src/lib"
)

type SessionIdWithContext struct {
	Id SessionId
	At time.Time
}

type FrontEnd struct {
	InFlight                map[uint64]SessionIdWithContext `json:"inFlight"`
	InFlightSessionToClient map[SessionId]uint64            `json:"inFlightSessionToClient"`
	NextSessionId           int                             `json:"nextSessionId"`
}

func NewFrontEnd() *FrontEnd {
	return &FrontEnd{map[uint64]SessionIdWithContext{}, map[SessionId]uint64{}, 0}
}

const register1 string = "register1"
const register2 string = "register2"

func (fe *FrontEnd) NewSessionId(clientId uint64, at time.Time) (SessionId, error) {
	var sessionId SessionId
	_, ok := fe.InFlight[clientId]

	if ok {
		return sessionId, fmt.Errorf("Client %d already has a session", clientId)
	}

	sessionId.Id = fe.NextSessionId
	fe.NextSessionId++
	fe.InFlight[clientId] = SessionIdWithContext{
		Id: sessionId,
		At: at,
	}
	fe.InFlightSessionToClient[sessionId] = clientId

	return sessionId, nil
}

func (fe *FrontEnd) RemoveSession(sessionId SessionId) (uint64, bool) {
	clientId, ok := fe.InFlightSessionToClient[sessionId]

	if ok {
		delete(fe.InFlight, clientId)
		delete(fe.InFlightSessionToClient, sessionId)
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

// When the frontend receives a client request for a write or a read, then it
// forwards it to both registers. A session id for that client is included in
// the forwarded requests and saved in the frontend, so that once we get a
// response from the registers we know which client the frontend should respond
// to.
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
			To:   lib.Singleton(register1),
			Args: translate(event.Request, sessionId),
		},
		{
			To:   lib.Singleton(register2),
			Args: translate(event.Request, sessionId),
		},
	}

	return oevs
}

// The frontend will forward the first response it gets from the registers to
// the client that made the request.
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
						To: lib.Singleton(
							fmt.Sprintf("client:%d", clientId)),
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

// TODO(stevan): Why was this necessary?
func (fe *FrontEnd) Tick(at time.Time) []lib.OutEvent {
	duration, _ := time.ParseDuration("25s")
	for key, value := range fe.InFlight {
		if at.After(value.At.Add(duration)) {
			delete(fe.InFlight, key)
		}
	}

	return nil
}

func (fe *FrontEnd) Timer(at time.Time) []lib.OutEvent {
	return nil
}

func (_ *FrontEnd) Init() []lib.OutEvent {
	return nil
}
