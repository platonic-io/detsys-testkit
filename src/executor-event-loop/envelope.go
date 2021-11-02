package executorEL

import (
	"encoding/json"
)

type EnvelopeKind int

const (
	Request = iota
	Response
)

func (t EnvelopeKind) String() string {
	return [...]string{"RequestKind", "ResponseKind"}[t]
}

func (t *EnvelopeKind) FromString(kind string) EnvelopeKind {
	return map[string]EnvelopeKind{
		"RequestKind":  Request,
		"ResponseKind": Response,
	}[kind]
}

func (t EnvelopeKind) MarshalJSON() ([]byte, error) {
	return json.Marshal(t.String())
}

func (t *EnvelopeKind) UnmarshalJSON(b []byte) error {
	var s string
	err := json.Unmarshal(b, &s)
	if err != nil {
		return err
	}
	*t = t.FromString(s)
	return nil
}

type LocalRef struct {
	Index int `json:"index"`
}

type RemoteRef struct {
	Address string `json:"address"`
	Index   int    `json:"index"`
}

func (r RemoteRef) ToLocal() LocalRef {
	return LocalRef{Index: r.Index}
}

type CorrelationId int
type LogicalTime int

func (lt *LogicalTime) Incr() {
	*lt = *lt + 1
}

func (lt *LogicalTime) Merge(olt LogicalTime) {
	lm := LogicalTime(-1)
	if int(*lt) < int(olt) {
		lm = olt
	} else {
		lm = *lt
	}
	*lt = lm + 1
}

// we need to agree how this should work
type Message struct {
	Kind    string          `json:"kind"`
	Message json.RawMessage `json:"message"` // is json value
}

type Envelope struct {
	Kind          EnvelopeKind  `json:"envelopeKind"`
	Sender        RemoteRef     `json:"envelopeSender"`
	Message       Message       `json:"envelopeMessage"`
	Receiver      RemoteRef     `json:"envelopeReceiver"`
	CorrelationId CorrelationId `json:"envelopeCorrelationId"`
	LogicalTime   LogicalTime   `json:"envelopeLogicalTime"`
}
