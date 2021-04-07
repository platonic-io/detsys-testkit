package debugger

import (
	"bytes"
	"encoding/json"
	"fmt"
	jsonpatch "github.com/evanphx/json-patch"
	_ "github.com/mattn/go-sqlite3"
	"github.com/nsf/jsondiff"
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
	SentAt    int
	To        string
	RecvAt    int
	Dropped   bool
	Simulated time.Time
}

func GetNetworkTrace(testId lib.TestId, runId lib.RunId) []NetworkEvent {
	db := lib.OpenDB()
	defer db.Close()

	rows, err := db.Query(`SELECT message,
                                      args,
                                      sender,
                                      sent_logical_time,
                                      receiver,
                                      recv_logical_time,
                                      dropped,
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
		err := rows.Scan(&event.Message, &event.Args, &event.From, &event.SentAt, &event.To, &event.RecvAt, &event.Dropped, (*lib.TimeFromString)(&event.Simulated))
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

type SequenceDiagrams struct {
	inner  map[int][]byte
	header []byte
	net    []NetworkEvent
}

func NewSequenceDiagrams(testId lib.TestId, runId lib.RunId) *SequenceDiagrams {
	net := GetNetworkTrace(testId, runId)
	return &SequenceDiagrams{
		inner: make(map[int][]byte),
		net:   net,
	}
}

func (s *SequenceDiagrams) At(at int) []byte {
	val, ok := s.inner[at]

	if ok {
		return val
	}

	arrows := make([]Arrow, 0, len(s.net))
	for _, event := range s.net {
		arrows = append(arrows, Arrow{
			From:    event.From,
			To:      event.To,
			Message: event.Message,
			Dropped: event.Dropped,
		})
	}
	header, gen := DrawDiagram(arrows, DrawSettings{
		MarkerSize: 3,
		MarkAt:     at,
	})

	if s.header == nil {
		s.header = header
	}

	s.inner[at] = gen
	return gen
}

func (s *SequenceDiagrams) Header() []byte {
	return s.header
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
