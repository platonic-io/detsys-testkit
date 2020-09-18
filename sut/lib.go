package sut

import (
	"time"
)

type Rpc interface{ Rpc() }

type reactor interface {
	Receive(at time.Time, from string, msg Rpc) []AddressedMessage
	Tick(at time.Time) []AddressedMessage
}

type AddressedMessage struct {
	Peer string `json:"peer"`
	Msg  Rpc    `json:"msg"`
}
