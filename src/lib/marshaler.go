package lib

import (
	"encoding/json"
	"fmt"
	"strconv"
	"strings"
	"time"
)

func UnmarshalScheduledEvent(m Marshaler, input []byte, output *ScheduledEvent) error {
	var s struct {
		At    time.Time       `json:"at"`
		From  string          `json:"from"`
		To    string          `json:"to"`
		Kind  string          `json:"kind"`
		Event string          `json:"event"`
		Args  json.RawMessage `json:"args"`
		Meta  MetaInfo        `json:"meta"`
	}
	if err := json.Unmarshal(input, &s); err != nil {
		return err
	}
	iev, err := unmarshal(m, s.Kind, s.Event, s.From, s.Args)
	if err != nil {
		return err
	}

	*output = ScheduledEvent{
		At:    s.At,
		From:  s.From,
		To:    s.To,
		Event: iev,
		Meta:  s.Meta,
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

func unmarshal(m Marshaler, kind string, event string, from string, input json.RawMessage) (InEvent, error) {
	var iev InEvent
	switch kind {
	case "invoke":
		var req Request
		if err := m.UnmarshalRequest(event, input, &req); err != nil {
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
		if err := m.UnmarshalMessage(event, input, &msg); err != nil {
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

type unscheduledEvent struct {
	To    []string `json:"to"`
	From  string   `json:"from"`
	Kind  string   `json:"kind"`
	Event string   `json:"event"`
	Args  Args     `json:"args"`
}
type timerEvent struct {
	Kind     string        `json:"kind"`
	Args     struct{}      `json:"args"`
	From     string        `json:"from"`
	Duration time.Duration `json:"duration-ns"`
}
type Event interface{ IsEvent() }

func (_ unscheduledEvent) IsEvent() {}
func (_ timerEvent) IsEvent()       {}

func OutEventsToEvents(from string, oevs []OutEvent) []Event {
	usevs := make([]Event, len(oevs))
	for index, oev := range oevs {
		var event Event
		switch kindT := oev.Args.(type) {
		case *ClientResponse:
			event = unscheduledEvent{
				From:  from,
				To:    oev.To,
				Kind:  "ok",
				Event: kindT.ResponseEvent(),
				Args:  oev.Args,
			}
		case *InternalMessage:
			event = unscheduledEvent{
				From:  from,
				To:    oev.To,
				Kind:  "message",
				Event: kindT.MessageEvent(),
				Args:  oev.Args,
			}
		case *Timer:
			event = timerEvent{
				Kind:     "timer",
				Args:     struct{}{},
				From:     from,
				Duration: kindT.Duration,
			}
		default:
			panic(fmt.Sprintf("%T", kindT))
		}
		usevs[index] = event
	}
	return usevs
}

func MarshalUnscheduledEvents(from string, corrId int, oevs []OutEvent) json.RawMessage {
	usevs := OutEventsToEvents(from, oevs)
	bs, err := json.Marshal(struct {
		Events []Event `json:"events"`
		CorrId int     `json:"corrId"`
	}{usevs, corrId})
	if err != nil {
		panic(err)
	}
	return bs
}
