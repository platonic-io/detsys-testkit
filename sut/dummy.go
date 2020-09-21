package sut

import (
	"encoding/json"
	"fmt"
	"time"

	"github.com/symbiont-io/detsys/lib"
)

type Node struct {
	state int
}

func NewNode() *Node {
	return &Node{
		state: 0,
	}
}

// type Req interface {
// 	Req()
// }

type Get struct {
	Id uint64 `json:"id"`
}

func (_ Get) Rpc() {}

func (n *Node) Parse(msg string, raw json.RawMessage) lib.Rpc {
	switch msg {
	case "get":
		var g Get
		// TODO(stevan): This allows unknown fields to be parsed, e.g.
		// `parameters:='{""foo": 1}'`. Probably need to do the same
		// thing as in DecodeJsonBody with custom Decoder...
		if err := json.Unmarshal(raw, &g); err != nil {
			// http.Error(w, "Cannot decode 'Get'", http.StatusBadRequest)
			// TODO(stevan): return custom error?
			fmt.Println("Cannot decode 'Get'")
			return nil
		}
		return g
		//	default:
		//		err := fmt.Sprintf("Unexpected 'Message': %s\n", msg.Message)
		//		http.Error(w, err, http.StatusInternalServerError)
		//		return
		//	}
	}
	return nil
}

type Inc struct{}

// func (g Get) Req()                         {}
// func (g Get) MarshalJSON() ([]byte, error) { return []byte(`{"op":"get"}`), nil }
// func (g Get) UnmarshalJSON(data []byte) error {
// 	// XXX: Remove all spaces from data?
// 	if bytes.Equal(data, []byte(`{"op":"get"}`)) {
// 		g = Get{}
// 		return nil
// 	} else {
// 		return errors.New("Get: UnmarshalJSON")
// 	}
// }

// func (g Inc) Req()                         {}
// func (g Inc) MarshalJSON() ([]byte, error) { return []byte(`{"op":"inc"}`), nil }

// type ClientRequest struct {
// 	Id      uint64 `json:"id"`
// 	Request Req    `json:"request"`
// }
//
// type ClientResponse struct {
// 	Id  uint64 `json:"id"`
// 	Val int    `json:"val"`
// }
//
// func (c ClientRequest) Rpc()  {}
// func (c ClientResponse) Rpc() {}
//
// var _ Rpc = ClientRequest{}
// var _ Rpc = ClientResponse{}

func (n *Node) Receive(_ time.Time, from string, msg lib.Rpc) []lib.AddressedMessage {
	var msgs []lib.AddressedMessage
	switch req := msg.(type) {
	case Get:
		msgs = []lib.AddressedMessage{
			{
				Peer: from,
				Msg:  Get{req.Id},
			},
		}
	default:
		return nil // XXX
	}
	return msgs
}

func (n *Node) Tick(_ time.Time) []lib.AddressedMessage {
	return nil
}

var _ lib.Reactor = &Node{}
