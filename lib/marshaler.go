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

func MarshalKind(args Args) string {
	switch kind := args.(type) {
	case *ClientResponse:
		return "ok"
	case *InternalMessage:
		return "message"
	default:
		panic(fmt.Sprintf("%T", kind))
	}
}

func MarshalUnscheduledEvents(m Marshaler, from string, oevs []OutEvent) json.RawMessage {
	usevs := make([]UnscheduledEvent, len(oevs))
	for index, oev := range oevs {
		usevs[index] = UnscheduledEvent{
			From:  from,
			To:    oev.To,
			Kind:  MarshalKind(oev.Args),
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
