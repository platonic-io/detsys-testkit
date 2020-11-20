package sut

import (
	"log"
	"time"

	"github.com/symbiont-io/detsys/lib"
)

type Register struct {
	Value    []int       `json:"value"`
	Sessions []SessionId `json:"sessions"`
}

func NewRegister() *Register {
	return &Register{
		Value:    []int{},
		Sessions: []SessionId{},
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
							Response: Value{r.Value},
						}},
					},
				}
			case Write:
				found := false
				for _, session := range r.Sessions {
					if session == msg.Id {
						found = true
						break
					}
				}
				if !found {
					r.Value = append(r.Value, imsg.Value)
					r.Sessions = append(r.Sessions, msg.Id)
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

func (r *Register) Timer(_ time.Time) []lib.OutEvent {
	return nil
}
