package executorEL

import (
	"encoding/json"
	"time"
)

type LogDirection int

const (
	LogSend = iota
	LogResumeContinuation
)

type LogEntry struct {
	LocalRef  LocalRef     `json:"local-ref"`
	RemoteRef RemoteRef    `json:"remote-ref"`
	Message   Message      `json:"message"`
	Direction LogDirection `json:"direction"`
}

type TimestampedLogEntry struct {
	LogEntry    LogEntry    `json:"entry"`
	LogicalTime LogicalTime `json:"logical-time"`
	Time        time.Time   `json:"time"`
}

// in the future will probably also take the name of event-loop
func (tle TimestampedLogEntry) Serialise() string {
	b, err := json.Marshal(tle)
	if err != nil {
		panic(err)
	}
	return string(b)
}
