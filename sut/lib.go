package sut

import (
	"time"
)

type rpc interface{ rpc() }

type reactor interface {
	receive(at time.Time, from string, msg rpc) []addressedMessage
	tick(at time.Time) []addressedMessage
}

type addressedMessage struct {
	peer string
	msg  rpc
}
