package debugger

import (
	"bytes"
	"database/sql"
	"encoding/json"
	"fmt"
	jsonpatch "github.com/evanphx/json-patch"
	_ "github.com/mattn/go-sqlite3"
	"github.com/nsf/jsondiff"

	"github.com/symbiont-io/detsys/lib"
)

type HeapDiff struct {
	Component string
	Diff      []byte
}

func GetInitHeap(testId lib.TestId) []HeapDiff {
	query := fmt.Sprintf(`SELECT component,args
                              FROM deployment
                              WHERE test_id = %d`, testId.TestId)
	return helper(query)
}

func GetHeapTrace(testId lib.TestId, runId lib.RunId) []HeapDiff {
	query := fmt.Sprintf(`SELECT component,heap
                              FROM heap_trace
                              WHERE test_id = %d AND run_id = %d`, testId.TestId, runId.RunId)
	return helper(query)
}

func helper(query string) []HeapDiff {
	db, err := sql.Open("sqlite3", "../../db/detsys.sqlite3")
	if err != nil {
		panic(err)
	}
	defer db.Close()

	// TODO(stevan): We are not using the `at` field from the database.
	rows, err := db.Query(query)
	if err != nil {
		panic(err)
	}
	defer rows.Close()

	var diffs []HeapDiff
	for rows.Next() {
		diff := HeapDiff{}
		err := rows.Scan(&diff.Component, &diff.Diff)
		if err != nil {
			panic(err)
		}
		diffs = append(diffs, diff)
	}
	return diffs
}

type NetworkEvent struct {
	Message string
	Args    []byte
	From    string
	To      string
	At      int
}

func GetNetworkTrace(testId lib.TestId, runId lib.RunId) []NetworkEvent {
	db, err := sql.Open("sqlite3", "../../db/detsys.sqlite3")
	if err != nil {
		panic(err)
	}
	defer db.Close()

	rows, err := db.Query("SELECT message,args,`from`,`to`,at FROM network_trace WHERE test_id = ? AND run_id = ?", testId.TestId, runId.RunId)
	if err != nil {
		panic(err)
	}
	defer rows.Close()

	var trace []NetworkEvent
	for rows.Next() {
		event := NetworkEvent{}
		err := rows.Scan(&event.Message, &event.Args, &event.From, &event.To, &event.At)
		if err != nil {
			panic(err)
		}
		trace = append(trace, event)
	}
	return trace
}

func applyDiff(original, diff []byte) []byte {
	new, err := jsonpatch.MergePatch(original, diff)
	if err != nil {
		panic(err)
	}
	return new
}

func PrettyJson(input []byte) []byte {
	var pretty bytes.Buffer
	err := json.Indent(&pretty, input, "", "  ")
	if err != nil {
		panic(err)
	}
	return pretty.Bytes()
}

func traceHeap(testId lib.TestId, runId lib.RunId) {
	inits := GetInitHeap(testId)
	changes := GetHeapTrace(testId, runId)
	network := GetNetworkTrace(testId, runId)

	heap := make(map[string][]byte)

	for _, init := range inits {
		heap[init.Component] = []byte(init.Diff)
		fmt.Printf("%s %s\n", init.Component, PrettyJson(heap[init.Component]))
	}
	for i, change := range changes {
		fmt.Printf("\n%s === %s %s ===> %s\n\n", network[i].From, network[i].Message,
			string(network[i].Args), network[i].To)
		old := heap[change.Component]
		new := applyDiff(old, change.Diff)
		heap[change.Component] = []byte(new)
		opts := jsondiff.DefaultConsoleOptions()
		opts.Indent = "  "
		_, strdiff := jsondiff.Compare(old, new, &opts)
		fmt.Printf("%s %s\n", change.Component, strdiff)
	}
}

func Heaps(testId lib.TestId, runId lib.RunId) []map[string][]byte {
	inits := GetInitHeap(testId)
	changes := GetHeapTrace(testId, runId)

	heaps := make([]map[string][]byte, len(changes)+1)

	m := make(map[string][]byte)
	for _, init := range inits {
		m[init.Component] = []byte(init.Diff)
	}
	heaps[0] = m

	for i, change := range changes {
		old := heaps[i][change.Component]
		new := applyDiff(old, change.Diff)
		m2 := make(map[string][]byte)
		m2[change.Component] = []byte(new)
		heaps[i+1] = m2
		for component, heap := range heaps[i] {
			if component != change.Component {
				heaps[i+1][component] = heap
			}
		}
	}
	return heaps
}
