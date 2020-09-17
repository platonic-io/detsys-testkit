package sut

import (
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

type req interface {
	req()
}

type Get struct{}
type Inc struct{}

func (g Get) req() {}
func (g Inc) req() {}

type ClientRequest struct {
	id      uint64
	request req
}

type ClientResponse struct {
	id  uint64
	val int
}

func (c ClientRequest) rpc()  {}
func (c ClientResponse) rpc() {}

var _ rpc = ClientRequest{}
var _ rpc = ClientResponse{}

func (n *Node) receive(_ time.Time, from string, msg rpc) []addressedMessage {
	return nil
}

func (n *Node) tick(_ time.Time) []addressedMessage {
	return nil
}

var _ reactor = &Node{}
