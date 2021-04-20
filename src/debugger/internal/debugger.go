package debugger

import (
	"bytes"
	"encoding/json"
	"errors"
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
	At      int
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
	db := lib.OpenDB()
	defer db.Close()

	rows, err := db.Query(
		`SELECT reactor,heap_diff, logical_time
                 FROM execution_step
                 WHERE test_id = ?
                 AND run_id = ?`, testId.TestId, runId.RunId)
	if err != nil {
		panic(err)
	}
	defer rows.Close()

	var diffs []HeapDiff
	for rows.Next() {
		diff := HeapDiff{}
		err := rows.Scan(&diff.Reactor, &diff.Diff, &diff.At)
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
		if !(event.Message == "timer" && event.Dropped) {
			trace = append(trace, event)
		}
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
	current := make(map[string][]byte)

	{
		first := make(map[string][]byte)
		for _, init := range inits {
			current[init.Reactor] = []byte(init.Diff)
			first[init.Reactor] = []byte(init.Diff)
		}
		heaps[0] = first
	}
	changeMap := make(map[int]HeapDiff)
	{
		for _, hd := range changes {
			changeMap[hd.At] = hd
		}
	}

	for i, event := range network {
		hd, found := changeMap[event.RecvAt]
		if found {
			old := current[hd.Reactor]
			current[hd.Reactor] = applyDiff(old, hd.Diff)

		}
		// we don't have a heap diff, could be because:
		// 1. The event was dropped
		// 2. The event was sent to a client
		// 3. SUT crashed
		// Either way we copy the previous state
		{
			cp := make(map[string][]byte)
			for k, v := range current {
				cp[k] = v
			}
			heaps[i+1] = cp

		}
	}
	return heaps
}

func colon(s string) string {
	return strings.Replace(s, ":", "", -1)
}

type SequenceDiagrams struct {
	inner   map[int][]byte
	header  []byte
	net     []NetworkEvent
	crashes CrashInformation
}

func NewSequenceDiagrams(testId lib.TestId, runId lib.RunId) *SequenceDiagrams {
	net := GetNetworkTrace(testId, runId)
	crashes := GetCrashes(testId, runId)
	return &SequenceDiagrams{
		inner:   make(map[int][]byte),
		net:     net,
		crashes: crashes,
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
			At:      event.RecvAt,
			Message: event.Message,
			Dropped: event.Dropped,
		})
	}
	header, gen := DrawDiagram(arrows, DrawSettings{
		MarkerSize: 3,
		MarkAt:     at,
		Crashes:    s.crashes,
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

func GetCrashes(testId lib.TestId, runId lib.RunId) CrashInformation {
	db := lib.OpenDB()
	defer db.Close()

	rows, err := db.Query(`SELECT faults
                               FROM run_info
                               WHERE test_id = ?
                               AND   run_id = ?`, testId.TestId, runId.RunId)
	if err != nil {
		panic(err)
	}
	defer rows.Close()
	var faults = make([]lib.Fault, 0)
	found_one := false
	for rows.Next() {
		if found_one {
			panic(errors.New(fmt.Sprintf("Found multiple runs with same id: %d - %d", testId.TestId, runId.RunId)))
		}
		found_one = true

		var jsonBlob []byte
		err := rows.Scan(&jsonBlob)
		if err != nil {
			panic(err)
		}
		err = json.Unmarshal(jsonBlob, &faults)
		if err != nil {
			panic(err)
		}
	}

	crashInformation := make(map[int][]string)
	for _, fault := range faults {
		switch ev := fault.Args.(type) {
		case lib.Crash:
			// assert (fault.Kind == "crash")
			crashesAtThatTime := crashInformation[ev.At] // will be empty array if key don't exists yet
			crashesAtThatTime = append(crashesAtThatTime, ev.From)
			crashInformation[ev.At] = crashesAtThatTime
		default:
		}
	}
	return crashInformation
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
