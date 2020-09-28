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

var un lib.Unmarshaler = &Un{}

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
	// input := []byte(`{"at":   "1970-01-01T00:00:00Z",
	//                   "from": "node1",
	//                   "to":   "node2",
	//                   "kind": "message"
	//                   "event":"ack",
	//                   "args": {}`)

	//	if expected != got {
	//		t.Errorf(`Contract with scheduler broken!\n
	//                          Cannot unmarshal incoming internal message event,
	//                          expected:\n%s, got:\n%s\n`, expected, got)
	//}
}

func TestSchedulerContractFault(t *testing.T) {
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
	// output := []lib.UnscheduledEvent{
	// 	lib.UnscheduledEvent{
	// 		To: "client:0",
	// 		Event: lib.ClientResponse{
	// 			Id:       0,
	// 			Response: Value{1},
	// 		},
	// 	},
	// 	// ClientFailResponse?
	// 	// ClientCrashResponse?
	// 	// InternalMessage
	// 	// TODO(stevan): Is that all outgoing events from the executor?
	// }
	// got := lib.MarshalUnscheduledEvent(output)
	// expected := []byte(`"from": "node",
	// 	            "to":   "client:0",
	// 	            "kind": "ok",
	//                     "event":"write",
	// 	            "args": {"value": 1}}`)

	// if not(bytes.Equal(expected, got)) {
	// 	t.Errorf(`Contract with Scheduler broken!\n
	//                   Cannot marshal outgoing unscheduled events,
	//                   expected:\n%s, got:\n%s\n`, expected, got)
	// }
}
