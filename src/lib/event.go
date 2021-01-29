package lib

import (
	"database/sql"
	"encoding/json"
)

func EmitEvent(db *sql.DB, event string, meta interface{}, data interface{}) {
	metaBlob, err := json.Marshal(meta)
	if err != nil {
		panic(err)
	}

	dataBlob, err := json.Marshal(data)
	if err != nil {
		panic(err)
	}

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
