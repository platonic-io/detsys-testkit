package debugger

import (
	"bytes"
	"encoding/json"
	"fmt"
	jsonpatch "github.com/evanphx/json-patch"
	_ "github.com/mattn/go-sqlite3"
	"github.com/nsf/jsondiff"
	"io/ioutil"
	"os"
	"os/exec"
	"strings"
	"time"

	"github.com/symbiont-io/detsys-testkit/src/lib"
)

type HeapDiff struct {
	Reactor string
	Diff    []byte
}

func GetInitHeap(testId lib.TestId) []HeapDiff {
	deploys, err := lib.DeploymentInfoForTest(testId)
	if err != nil {
		panic(err)
	}

	diffs := make([]HeapDiff, 0, len(deploys))
	for _, dep := range deploys {
		diffs = append(diffs, HeapDiff{
			Reactor: dep.Reactor,
			Diff:    dep.Args,
		})
	}

	return diffs
}

func GetHeapTrace(testId lib.TestId, runId lib.RunId) []HeapDiff {
	query := fmt.Sprintf(`SELECT reactor,heap_diff
                              FROM execution_step
                              WHERE test_id = %d
                                AND run_id = %d`, testId.TestId, runId.RunId)
	return helper(query)
}

func helper(query string) []HeapDiff {
	db := lib.OpenDB()
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
		err := rows.Scan(&diff.Reactor, &diff.Diff)
		if err != nil {
			panic(err)
		}
		diffs = append(diffs, diff)
	}
	return diffs
}

type NetworkEvent struct {
	Message   string
	Args      []byte
	From      string
	To        string
	Dropped   bool
	At        int
	Simulated time.Time
}

func GetNetworkTrace(testId lib.TestId, runId lib.RunId) []NetworkEvent {
	db := lib.OpenDB()
	defer db.Close()

	rows, err := db.Query(`SELECT message,
                                      args,
                                      sender,
                                      receiver,
                                      dropped,
                                      recv_logical_time,
                                      recv_simulated_time
		               FROM network_trace
		               WHERE test_id = ?
		                 AND run_id = ?`, testId.TestId, runId.RunId)
	if err != nil {
		panic(err)
	}
	defer rows.Close()

	var trace []NetworkEvent
	for rows.Next() {
		event := NetworkEvent{}
		err := rows.Scan(&event.Message, &event.Args, &event.From, &event.To, &event.Dropped, &event.At, (*lib.TimeFromString)(&event.Simulated))
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
		heap[init.Reactor] = []byte(init.Diff)
		fmt.Printf("%s %s\n", init.Reactor, PrettyJson(heap[init.Reactor]))
	}
	for i, change := range changes {
		fmt.Printf("\n%s === %s %s ===> %s\n\n", network[i].From, network[i].Message,
			string(network[i].Args), network[i].To)
		old := heap[change.Reactor]
		new := applyDiff(old, change.Diff)
		heap[change.Reactor] = []byte(new)
		opts := jsondiff.DefaultConsoleOptions()
		opts.Indent = "  "
		_, strdiff := jsondiff.Compare(old, new, &opts)
		fmt.Printf("%s %s\n", change.Reactor, strdiff)
	}
}

func Max(x, y int) int {
	if x < y {
		return y
	}
	return x
}

func Heaps(testId lib.TestId, runId lib.RunId) []map[string][]byte {
	inits := GetInitHeap(testId)
	changes := GetHeapTrace(testId, runId)
	network := GetNetworkTrace(testId, runId)

	heaps := make([]map[string][]byte, len(network)+1)

	m := make(map[string][]byte)
	for _, init := range inits {
		m[init.Reactor] = []byte(init.Diff)
	}
	heaps[0] = m

	dropped := 0
	for i, event := range network {
		if event.Dropped || strings.HasPrefix(event.To, "client") {
			dropped++
		}
		// The `Max` below is needed in case the first message is
		// dropped.
		j := Max(0, i-dropped)

		old := heaps[i][changes[j].Reactor]
		new := applyDiff(old, changes[j].Diff)
		m2 := make(map[string][]byte)
		m2[changes[j].Reactor] = []byte(new)
		heaps[i+1] = m2
		for reactor, heap := range heaps[i] {
			if reactor != changes[j].Reactor {
				heaps[i+1][reactor] = heap
			}
		}
	}
	return heaps
}

func colon(s string) string {
	return strings.Replace(s, ":", "", -1)
}

func SequenceDiagrams(testId lib.TestId, runId lib.RunId) [][]byte {
	tmpfile, err := ioutil.TempFile("", "sequence_diagram_*_000.txt")
	if err != nil {
		panic(err)
	}
	net := GetNetworkTrace(testId, runId)
	for i := 0; i < len(net); i++ {
		if _, err := tmpfile.Write([]byte("@startuml\n")); err != nil {
			tmpfile.Close()
			panic(err)
		}
		for j, event := range net {
			var arrow string
			if event.Dropped {
				arrow = "-->"
			} else {
				arrow = "->"
			}
			var line string
			if i == j {
				line = fmt.Sprintf("%s %s %s : <<< %s >>>\n",
					colon(event.From), arrow, colon(event.To), event.Message)
			} else {
				line = fmt.Sprintf("%s %s %s : %s\n",
					colon(event.From), arrow, colon(event.To), event.Message)
			}
			if _, err := tmpfile.Write([]byte(line)); err != nil {
				tmpfile.Close()
				panic(err)
			}
		}
		if _, err := tmpfile.Write([]byte("@enduml\n")); err != nil {
			tmpfile.Close()
			panic(err)
		}
	}

	cmd := exec.Command("plantuml", "-tutxt", tmpfile.Name())
	err = cmd.Run()
	if err != nil {
		panic(err)
	}

	base := strings.TrimSuffix(tmpfile.Name(), ".txt")

	diagrams := make([][]byte, len(net))
	var file string

	for i := 0; i < len(net); i++ {
		if i == 0 {
			file = base + ".utxt"
		} else {
			file = fmt.Sprintf("%s_%03d.utxt", base, i)
		}
		diagram, err := ioutil.ReadFile(file)
		if err != nil {
			panic(err)
		}
		diagrams[i] = []byte(diagram)
		os.Remove(file)
	}
	defer os.Remove(tmpfile.Name())

	return diagrams
}

func GetLogMessages(testId lib.TestId, runId lib.RunId, reactor string, at int) [][]byte {
	db := lib.OpenDB()
	defer db.Close()

	rows, err := db.Query(`SELECT log_lines
                               FROM execution_step
                               WHERE test_id = ?
                               AND   run_id = ?
                               AND   reactor = ?
                               AND   logical_time = ? `,
		testId.TestId, runId.RunId, reactor, at)
	if err != nil {
		panic(err)
	}
	defer rows.Close()
	var logs [][]byte
	for rows.Next() {
		var jsonlog []byte
		err := rows.Scan(&jsonlog)

		if err != nil {
			panic(err)
		}

		if jsonlog == nil {
			continue
		}

		var log []string
		err = json.Unmarshal(jsonlog, &log)

		if err != nil {
			panic(err)
		}

		for _, entry := range log {
			logs = append(logs, []byte(entry))
		}

	}

	return logs
}
