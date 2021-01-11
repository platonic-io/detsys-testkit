package sut

import (
	"fmt"
	"log"
	"time"

	"github.com/symbiont-io/detsys-testkit/src/lib"
)

type FrontEnd4 struct {
	OnGoing       []OnGoingInternalRequest
	InFlight      map[SessionId]uint64
	NextSessionId int
	ResendTimer   time.Duration
}

func NewFrontEnd4() *FrontEnd4 {
	resendTimer, _ := time.ParseDuration("5s")
	return &FrontEnd4{[]OnGoingInternalRequest{}, map[SessionId]uint64{}, 0, resendTimer}
}

func (fe *FrontEnd4) ReceiveClient(at time.Time, from string, event lib.ClientRequest) []lib.OutEvent {
	sessionId := SessionId{fe.NextSessionId}
	fe.NextSessionId++

	fe.InFlight[sessionId] = event.Id

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
			To:   register1,
			Args: args,
		},
		{
			To:   register2,
			Args: args,
		},
		{
			To: "frontend",
			Args: &lib.Timer{
				Duration: fe.ResendTimer,
			},
		},
	}
}

func (fe *FrontEnd4) RemoveSession(from string, sessionId SessionId) (uint64, bool) {
	clientId, ok := fe.InFlight[sessionId]

	noMore := true
	found := false
	if ok {
		temp := fe.OnGoing[:0]

		for _, on := range fe.OnGoing {
			if on.SessionId != sessionId {
				temp = append(temp, on)
			} else if from != on.Register {
				noMore = false
				temp = append(temp, on)
			} else {
				found = true
			}
		}

		if noMore {
			delete(fe.InFlight, sessionId)
		}

		fe.OnGoing = temp
	}

	return clientId, (noMore && found)
}

func (fe *FrontEnd4) Receive(at time.Time, from string, event lib.InEvent) []lib.OutEvent {
	var oevs []lib.OutEvent
	switch ev := event.(type) {
	case *lib.ClientRequest:
		oevs = fe.ReceiveClient(at, from, *ev)
	case *lib.InternalMessage:
		switch msg := (*ev).Message.(type) {
		case InternalResponse:
			clientId, noMore := fe.RemoveSession(from, msg.Id)
			if noMore {
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

func (_ *FrontEnd4) Tick(at time.Time) []lib.OutEvent {
	return nil
}

func (fe *FrontEnd4) Timer(at time.Time) []lib.OutEvent {
	resend := []lib.OutEvent{}

	for i, on := range fe.OnGoing {
		if at.After(on.At.Add(fe.ResendTimer)) {
			fe.OnGoing[i].At = at
			fe.OnGoing[i].NumberOfTries++
			event := lib.OutEvent{
				To:   on.Register,
				Args: translate(on.Request, on.SessionId),
			}
			resend = append(resend, event)
		}
	}

	if len(resend) > 0 {
		resend = append(resend, lib.OutEvent{
			To: "frontend",
			Args: &lib.Timer{
				Duration: fe.ResendTimer,
			},
		})
	}

	return resend
}

func (_ *FrontEnd4) Init() []lib.OutEvent {
	return nil
}
