package sut

import (
	"bytes"
	"errors"
	"time"
)

type Node struct {
	state int
}

func NewNode() *Node {
	return &Node{
		state: 0,
	}
}

type Req interface {
	Req()
}

type Get struct{}
type Inc struct{}

func (g Get) Req()                         {}
func (g Get) MarshalJSON() ([]byte, error) { return []byte(`{"op":"get"}`), nil }
func (g Get) UnmarshalJSON(data []byte) error {
	// XXX: Remove all spaces from data?
	if bytes.Equal(data, []byte(`{"op":"get"}`)) {
		g = Get{}
		return nil
	} else {
		return errors.New("Get: UnmarshalJSON")
	}
}

func (g Inc) Req()                         {}
func (g Inc) MarshalJSON() ([]byte, error) { return []byte(`{"op":"inc"}`), nil }

type ClientRequest struct {
	Id      uint64 `json:"id"`
	Request Req    `json:"request"`
}

type ClientResponse struct {
	Id  uint64 `json:"id"`
	Val int    `json:"val"`
}

func (c ClientRequest) Rpc()  {}
func (c ClientResponse) Rpc() {}

var _ Rpc = ClientRequest{}
var _ Rpc = ClientResponse{}

func (n *Node) Receive(_ time.Time, from string, msg Rpc) []AddressedMessage {
	var msgs []AddressedMessage
	msgs = []AddressedMessage{
		{
			Peer: from,
			Msg: ClientResponse{
				Id:  0,
				Val: 0,
			},
		},
	}
	return msgs
}

func (n *Node) Tick(_ time.Time) []AddressedMessage {
	return nil
}

var _ reactor = &Node{}
