package executor

import (
	"database/sql"
	"encoding/json"
	"time"

	"github.com/symbiont-io/detsys-testkit/src/lib"
)

// We should probably have what kind of step this is, message vs. timer
type ExecutionStepEvent struct {
	Meta          lib.MetaInfo
	Reactor       string
	SimulatedTime time.Time
	LogLines      []string
	HeapDiff      json.RawMessage
}

func EmitExecutionStepEvent(db *sql.DB, event ExecutionStepEvent) {
	metaBlob, err := json.Marshal(struct {
		Component string     `json:"component"`
		RunId     lib.RunId  `json:"run-id"`
		TestId    lib.TestId `json:"test-id"`
	}{
		Component: "executor",
		RunId:     event.Meta.RunId,
		TestId:    event.Meta.TestId,
	})
	if err != nil {
		panic(err)
	}

	dataBlob, err := json.Marshal(struct {
		Reactor       string          `json:"reactor"`
		LogicalTime   int             `json:"logical-time"`
		SimulatedTime time.Time       `json:"simulated-time"`
		LogLines      []string        `json:"log-lines"`
		HeapDiff      json.RawMessage `json:"diff"`
	}{
		Reactor:       event.Reactor,
		LogicalTime:   event.Meta.LogicalTime,
		SimulatedTime: event.SimulatedTime,
		LogLines:      event.LogLines,
		HeapDiff:      event.HeapDiff,
	})
	if err != nil {
		panic(err)
	}

	stmt, err := db.Prepare(`INSERT INTO event_log(event, meta, data) VALUES(?,?,?)`)
	if err != nil {
		panic(err)
	}
	defer stmt.Close()

	_, err = stmt.Exec("ExecutionStep", metaBlob, dataBlob)

	if err != nil {
		panic(err)
	}
}
