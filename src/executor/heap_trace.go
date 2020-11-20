package executor

import (
	"database/sql"
	"encoding/json"
	jsonpatch "github.com/evanphx/json-patch"
	_ "github.com/mattn/go-sqlite3"
	"time"

	"github.com/symbiont-io/detsys/lib"
)

func dumpHeapJson(component interface{}) []byte {
	heap, err := json.Marshal(component)
	if err != nil {
		panic(err)
	}
	return heap
}

func jsonDiff(original []byte, modified []byte) []byte {
	diff, err := jsonpatch.CreateMergePatch(original, modified)
	if err != nil {
		panic(err)
	}
	return diff
}

func appendHeapTrace(db *sql.DB, testId lib.TestId, component string, diff []byte, at time.Time) {
	stmt, err := db.Prepare("SELECT MAX(id) FROM run WHERE test_id = ?")
	if err != nil {
		panic(err)
	}
	defer stmt.Close()

	var runId lib.RunId
	if err := stmt.QueryRow(testId.TestId).Scan(&runId.RunId); err != nil {
		panic(err)
	}

	stmt, err = db.Prepare(`INSERT INTO heap_trace(test_id, run_id, id, component, heap, at)
                                VALUES(?, ?,
                                  (SELECT IFNULL(MAX(id), -1) + 1 FROM heap_trace
                                   WHERE test_id = ?
                                   AND   run_id  = ?),
                                  ?, ?, ?)`)
	if err != nil {
		panic(err)
	}
	defer stmt.Close()

	_, err = stmt.Exec(testId.TestId, runId.RunId, testId.TestId, runId.RunId, component, diff, at)
	if err != nil {
		panic(err)
	}
}
