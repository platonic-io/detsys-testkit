package lib

import (
	"encoding/json"
)

type EventLogEmitter struct {
	Component string
	RunId     *RunId  // maybe we have one
	TestId    *TestId // maybe we have one
}

func (meta *EventLogEmitter) EmitData(event string, state string, data interface{}) {
	metaBlob, err := json.Marshal(struct {
		State     string  `json:"state"`
		Component string  `json:"component"`
		RunId     *RunId  `json:"run-id"`
		TestId    *TestId `json:"test-id"`
	}{
		State:     state,
		Component: meta.Component,
		RunId:     meta.RunId,
		TestId:    meta.TestId,
	})
	if err != nil {
		panic(err)
	}

	dataBlob, err := json.Marshal(data)
	if err != nil {
		panic(err)
	}

	db := OpenDB()
	defer db.Close()

	stmt, err := db.Prepare(`INSERT INTO event_log(event, meta, data) VALUES(?,?,?)`)
	if err != nil {
		panic(err)
	}
	defer stmt.Close()

	_, err = stmt.Exec(event, metaBlob, dataBlob)

	if err != nil {
		panic(err)
	}
}

func (emitter *EventLogEmitter) Emit(event string, state string) {
	emitter.EmitData(event, state, struct{}{})
}
