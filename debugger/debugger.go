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
	Dropped bool
	At      int
}

func GetNetworkTrace(testId lib.TestId, runId lib.RunId) []NetworkEvent {
	db := lib.OpenDB()
	defer db.Close()

	// TODO(stevan): Deal with dropped and client responses properly...
	rows, err := db.Query("SELECT message,args,`from`,`to`,dropped,at FROM network_trace WHERE test_id = ? AND run_id = ? AND NOT(`to` LIKE 'client:%')", testId.TestId, runId.RunId)
	if err != nil {
		panic(err)
	}
	defer rows.Close()

	var trace []NetworkEvent
	for rows.Next() {
		event := NetworkEvent{}
		err := rows.Scan(&event.Message, &event.Args, &event.From, &event.To, &event.Dropped, &event.At)
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
	network := GetNetworkTrace(testId, runId)

	heaps := make([]map[string][]byte, len(network)+1)

	m := make(map[string][]byte)
	for _, init := range inits {
		m[init.Component] = []byte(init.Diff)
	}
	heaps[0] = m

	dropped := 0
	for i, event := range network {
		if event.Dropped {
			dropped++
			old := heaps[i][changes[i-1].Component]
			new := applyDiff(old, changes[i-1].Diff)
			m2 := make(map[string][]byte)
			m2[changes[i-1].Component] = []byte(new)
			heaps[i+1] = m2
			for component, heap := range heaps[i] {
				if component != changes[i-1].Component {
					heaps[i+1][component] = heap
				}
			}
		} else {
			old := heaps[i][changes[i-dropped].Component]
			new := applyDiff(old, changes[i-dropped].Diff)
			m2 := make(map[string][]byte)
			m2[changes[i-dropped].Component] = []byte(new)
			heaps[i+1] = m2
			for component, heap := range heaps[i] {
				if component != changes[i-dropped].Component {
					heaps[i+1][component] = heap
				}
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
