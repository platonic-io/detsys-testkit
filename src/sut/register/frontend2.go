package sut

import (
	"fmt"
	"log"
	"time"

	"github.com/symbiont-io/detsys-testkit/src/lib"
)

type OnGoingInternalRequest struct {
	Register      string
	Request       lib.Request
	SessionId     SessionId
	At            time.Time
	NumberOfTries int
}

type FrontEnd2 struct {
	OnGoing          []OnGoingInternalRequest
	InFlight         map[SessionId]uint64
	ReceivedResponse map[SessionId]bool
	NextSessionId    int
}

func NewFrontEnd2() *FrontEnd2 {
	return &FrontEnd2{[]OnGoingInternalRequest{}, map[SessionId]uint64{}, map[SessionId]bool{}, 0}
}

func (fe *FrontEnd2) ReceiveClient(at time.Time, from string, event lib.ClientRequest) []lib.OutEvent {
	sessionId := SessionId{fe.NextSessionId}
	fe.NextSessionId++

	fe.InFlight[sessionId] = event.Id
	fe.ReceivedResponse[sessionId] = false

	args := translate(event.Request, sessionId)

	fe.OnGoing = append(fe.OnGoing, OnGoingInternalRequest{
		Register:      register1,
		Request:       event.Request,
		SessionId:     sessionId,
		At:            at,
		NumberOfTries: 0,
	})
	fe.OnGoing = append(fe.OnGoing, OnGoingInternalRequest{
		Register:      register2,
		Request:       event.Request,
		SessionId:     sessionId,
		At:            at,
		NumberOfTries: 0,
	})

	return []lib.OutEvent{
		{
			To:   lib.Singleton(register1),
			Args: args,
		},
		{
			To:   lib.Singleton(register2),
			Args: args,
		},
	}
}

func (fe *FrontEnd2) RemoveSession(sessionId SessionId) (uint64, bool) {
	clientId, ok := fe.InFlight[sessionId]

	if ok {
		delete(fe.InFlight, sessionId)

		temp := fe.OnGoing[:0]

		for _, on := range fe.OnGoing {
			if on.SessionId != sessionId {
				temp = append(temp, on)
			}
		}

		fe.OnGoing = temp
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
			if new, ok := fe.ReceivedResponse[msg.Id]; !new || !ok {
				fe.ReceivedResponse[msg.Id] = true
				break
			}
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

func (fe *FrontEnd2) Tick(at time.Time) []lib.OutEvent {
	resend := []lib.OutEvent{}

	resendTimer, _ := time.ParseDuration("5s")

	for i, on := range fe.OnGoing {
		if at.After(on.At.Add(resendTimer)) {
			fe.OnGoing[i].At = at
			fe.OnGoing[i].NumberOfTries++
			event := lib.OutEvent{
				To:   lib.Singleton(on.Register),
				Args: translate(on.Request, on.SessionId),
			}
			resend = append(resend, event)
		}
	}

	return resend
}

func (_ *FrontEnd2) Timer(at time.Time) []lib.OutEvent {
	return nil
}

func (_ *FrontEnd2) Init() []lib.OutEvent {
	return nil
}
