package sut

import (
	"encoding/json"
	"fmt"
	"strconv"
	"strings"

	"github.com/symbiont-io/detsys/lib"
)

type Marshaler struct{}

func NewMarshaler() *Marshaler {
	return &Marshaler{}
}

func (_ *Marshaler) UnmarshalRequest(request string, raw json.RawMessage, req *lib.Request) error {
	switch strings.ToLower(request) {
	case "read":
		var op Read
		if err := json.Unmarshal(raw, &op); err != nil {
			return err
		}
		*req = op
	case "write":
		var op Write
		if err := json.Unmarshal(raw, &op); err != nil {
			return err
		}
		*req = op
	default:
		return fmt.Errorf("Unknown request type: %s\n%s", request, raw)
	}
	return nil
}

func (_ *Marshaler) UnmarshalResponse(response string, raw json.RawMessage) lib.Response {
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

func (_ *Marshaler) UnmarshalMessage(message string, raw json.RawMessage, msg *lib.Message) error {
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

func (_ *Marshaler) MarshalRequest(request lib.Request) string {
	switch ev := request.(type) {
	case Read:
		return "read"
	case Write:
		return "write"
	default:
		panic(fmt.Errorf("Unknown Request event: %#v\n", ev))
	}
}

func (_ *Marshaler) MarshalResponse(response lib.Response) string {
	switch ev := response.(type) {
	case Ack:
		return "ack"
	case Value:
		return "value"
	default:
		panic(fmt.Errorf("Unknown Response event: %#v\n", ev))
	}
}

func (m *Marshaler) MarshalEvent(event lib.Args) string {
	switch ev := event.(type) {
	case *lib.InternalMessage:
		switch msg := (*ev).Message.(type) {
		case InternalRequest:
			return m.MarshalRequest(msg.Request)
		case InternalResponse:
			return m.MarshalResponse(msg.Response)
		default:
			panic(fmt.Errorf("Unknown type of InternalMessage: %#v\n", msg))
		}
	case *lib.ClientResponse:
		return m.MarshalResponse((*ev).Response)

	default:
		panic(fmt.Errorf("Unknown type to marshal: %#v\n", ev))
	}
}
