package executor

import (
	"bytes"
	"encoding/json"
	"testing"
	"time"
)

func TestJsonEncoding(t *testing.T) {
	msg := ScheduledMessage{
		At:      time.Date(1970, 1, 1, 0, 0, 0, 0, time.UTC),
		From:    "client",
		To:      "node",
		Message: "get",
		Rpc: struct {
			Id int `json:"id"`
		}{
			Id: 1,
		},
	}
	expected := []byte(`{"at":"1970-01-01T00:00:00Z","from":"client","to":"node","command":"get","parameters":{"id":1}}`)
	got, _ := json.Marshal(msg)
	if !bytes.Equal(expected, got) {
		t.Errorf("Incorrect JSON encoding, expected:\n%s, got:\n%s.", expected, got)
	}

}
