package broadcast

import (
	"encoding/json"
	"fmt"
	"strings"

	"github.com/symbiont-io/detsys-testkit/src/lib"
)

type Marshaler struct{}

func NewMarshaler() *Marshaler {
	return &Marshaler{}
}

func (_ *Marshaler) UnmarshalRequest(request string, raw json.RawMessage, req *lib.Request) error {
	return fmt.Errorf("Unknown request type: %s\n%s", request, raw)
}

func (_ *Marshaler) UnmarshalResponse(response string, raw json.RawMessage) lib.Response {
	panic(fmt.Sprintf("Unknown request type: %s\n%s", response, raw))
}

func (_ *Marshaler) UnmarshalMessage(message string, raw json.RawMessage, msg *lib.Message) error {
	switch strings.ToLower(message) {
	case "broadcast":
		var op struct {
			Data string `json:"data"`
		}
		if err := json.Unmarshal(raw, &op); err != nil {
			panic(err)
		}
		*msg = Broadcast{
			Data: op.Data,
		}
	default:
		panic(fmt.Errorf("Unknown message type: %s\n%s", message, raw))
	}
	return nil
}
