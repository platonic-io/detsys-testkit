package lib

import (
	"encoding/json"
	"time"
)

type Rpc interface{ Rpc() }

type Reactor interface {
	Receive(at time.Time, from string, msg Rpc) []AddressedMessage
	Tick(at time.Time) []AddressedMessage
	Parse(msg string, raw json.RawMessage) Rpc
}

type AddressedMessage struct {
	To         string `json:"to"`
	Command    string `json:"command"`
	Parameters Rpc    `json:"parameters"`
}
