package executor

import (
	"encoding/json"
	"fmt"
	"reflect"
	"testing"
	"time"

	"github.com/symbiont-io/detsys/lib"
)

// ---------------------------------------------------------------------

type Write struct {
	Value int `json:"value"`
}

func (_ Write) Request() {}

type Ack struct{}

func (_ Ack) Message() {}

type Value struct {
	Value int `json:"value"`
}

func (_ Value) Response() {}

type Un struct{}
type M struct{}

func (un *Un) ParseRequest(req string, input json.RawMessage, output *lib.Request) error {
	switch req {
	case "write":
		var w Write
		if err := json.Unmarshal(input, &w); err != nil {
			return err
		}
		*output = w
	}
	return nil
}

func (un *Un) ParseMessage(msg string, input json.RawMessage, output *lib.Message) error {
	switch msg {
	case "ack":
		*output = Ack{}
	}
	return nil
	//var resp Response
	//switch msg {
	//case "Value":
	//var v Value
	//if err := json.Unmarshal(raw, &v); err != nil {
	//panic(err)
	//}
	//resp = v
	//}
	//return resp
}

// Shouldn't part of this code be in the lib?
func (m *M) MarshalEvent(args lib.Args) string {
	switch event := args.(type) {
	case *lib.ClientResponse:
		return "write"
	case *lib.InternalMessage:
		return "ack"
	default:
		panic(fmt.Sprintf("Unknown event: %T", event))
	}
}

var un lib.Unmarshaler = &Un{}
var m lib.Marshaler = &M{}

// ---------------------------------------------------------------------
// Ensure that incoming events can be unmarshaled.

func equal(t *testing.T, msg string, expected lib.ScheduledEvent, got lib.ScheduledEvent) {
	if !reflect.DeepEqual(expected, got) {
		t.Error("The contract with Scheduler is broken!\n" +
			msg + ", expected:\n" +
			fmt.Sprintf("%+v", expected) + ", where Event is:\n" +
			fmt.Sprintf("%+v", expected.Event) + ", got:\n" +
			fmt.Sprintf("%+v\n", got) + ", where Event is:\n" +
			fmt.Sprintf("%+v", got.Event))
	}
}

func equalUS(t *testing.T, msg string, expected json.RawMessage, got json.RawMessage) {
	var oexpected interface{}
	var ogot interface{}

	if err := json.Unmarshal(expected, &oexpected); err != nil {
		t.Error(err)
	}

	if err := json.Unmarshal(got, &ogot); err != nil {
		t.Error(err)
	}

	if !reflect.DeepEqual(oexpected, ogot) {
		t.Error("The contract with Scheduler is broken!\n" +
			msg + ", expected:\n" +
			fmt.Sprintf("%+v", oexpected) + ", got:\n" +
			fmt.Sprintf("%+v\n", ogot))
	}
}

func TestSchedulerContractClientRequest(t *testing.T) {
	input := []byte(`{"at":   "1970-01-01T00:00:00Z",
                          "from": "client:0",
                          "to":   "node",
                          "kind": "invoke",
                          "event":"write",
                          "args": {"value": 1}}`)

	var got lib.ScheduledEvent
	if err := lib.UnmarshalScheduledEvent(un, input, &got); err != nil {
		t.Error(err)
		return
	}
	expected := lib.ScheduledEvent{
		At:   time.Date(1970, 1, 1, 0, 0, 0, 0, time.UTC),
		To:   "node",
		From: "client:0",
		Event: &lib.ClientRequest{
			Id:      0,
			Request: Write{1},
		},
	}
	equal(t, "Cannot unmarshal incoming client request event", expected, got)
}

func TestSchedulerContractInternalMessage(t *testing.T) {
	input := []byte(`{"at":   "1970-01-01T00:00:00Z",
	                  "from": "node1",
	                  "to":   "node2",
	                  "kind": "message",
	                  "event":"ack",
	                  "args": {}}`)

	var got lib.ScheduledEvent
	if err := lib.UnmarshalScheduledEvent(un, input, &got); err != nil {
		t.Error(err)
		return
	}
	expected := lib.ScheduledEvent{
		At:   time.Date(1970, 1, 1, 0, 0, 0, 0, time.UTC),
		To:   "node2",
		From: "node1",
		Event: &lib.InternalMessage{
			Message: Ack{}},
	}

	equal(t, "Cannot unmarshal incoming internal message event", expected, got)
}

func TestSchedulerContractFault(t *testing.T) {
	t.Skip("to be implemented later")
	// input := []byte(`{"at":   "1970-01-01T00:00:00Z",
	//                   "from": "environment",
	//                   "to":   "executor",
	//                   "kind": "fault"
	//                   "event":"kill",
	//                   "args": {"components": ["node1", "node2"]}}`)
}

// TODO(stevan): Is that all incoming events an executor can get?

// ---------------------------------------------------------------------
// Ensure that outgoing unscheduled events can be marshaled.

func TestSchedulerContractOutput(t *testing.T) {
	output := []lib.OutEvent{
		lib.OutEvent{
			To: "client:0",
			Args: &lib.ClientResponse{
				Id:       0,
				Response: Value{1},
			},
		},
		// ClientFailResponse?
		// ClientCrashResponse?
		// InternalMessage
		// TODO(stevan): Is that all outgoing events from the executor?
	}
	got := lib.MarshalUnscheduledEvents(m, "node", output)
	expected := []byte(`{"events":
                              [{"from": "node",
		                "to":   "client:0",
		                "kind": "ok",
	                        "event":"write",
		                "args": {"id": 0,
                                         "response": {"value": 1}}}]}`)

	equalUS(t, "Cannot mashal outgoing unscheduled events", expected, got)
}
