package executorEL

import (
	"fmt"
	"strings"
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
	le := tle.LogEntry
	em := strings.ReplaceAll(string(le.Message.Message), "\"", "\\\"")
	lr := fmt.Sprintf("(LocalRef %d)", le.LocalRef.Index)
	rr := fmt.Sprintf("(RemoteRef {address = %#v, index = %d})", le.RemoteRef.Address, le.RemoteRef.Index)
	m := fmt.Sprintf("(InternalMessage \"%s\")", em)
	lt := fmt.Sprintf("(LogicalTime (NodeName \"executor\") %d)", int(tle.LogicalTime))
	t := fmt.Sprintf("(Time %s)", tle.Time.UTC().Format("2006-01-02 15:04:05.00000 MST"))
	switch le.Direction {
	case LogSend:
		return fmt.Sprintf("Timestamped (LogSend %s %s %s) %s %s", lr, rr, m, lt, t)
	case LogResumeContinuation:
		return fmt.Sprintf("Timestamped (LogResumeContinuation %s %s %s) %s %s", rr, lr, m, lt, t)
	default:
		panic("Unknown direction in log")
	}
}
