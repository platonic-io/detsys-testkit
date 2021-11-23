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
	LocalRef  LocalRef
	RemoteRef RemoteRef
	Message   Message
	Direction LogDirection
}

func (le LogEntry) MarshalJSON() ([]byte, error) {
	var tag string
	switch le.Direction {
	case LogSend:
		tag = "LogSend"
	case LogResumeContinuation:
		tag = "LogResumeContinuation"
	}
	j, err := json.Marshal(struct {
		LocalRef  int       `json:"localRef"`
		RemoteRef RemoteRef `json:"remoteRef"`
		Message   Message   `json:"message"`
		Tag       string    `json:"tag"`
	}{le.LocalRef.Index, le.RemoteRef, le.Message, tag})
	return j, err
}

type TimestampedLogEntry struct {
	LogEntry    LogEntry    `json:"content"`
	LogicalTime LogicalTime `json:"logicalTime"`
	Time        time.Time   `json:"time"`
}
