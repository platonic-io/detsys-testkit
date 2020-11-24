package debugger

import (
	"bytes"
	"encoding/json"
	"fmt"
	"strconv"
	"testing"

	"github.com/symbiont-io/detsys-testkit/lib"
)

type FrontEnd struct {
	InFlight                map[uint64]SessionId `json:"inFlight"`
	InFlightSessionToClient map[SessionId]uint64 `json:"inFlightSessionToClient"`
	NextSessionId           int                  `json:"nextSessionId"`
}

type SessionId struct {
	Id int `json:"id"`
}

func (sid SessionId) MarshalText() ([]byte, error) {
	return []byte(strconv.Itoa(sid.Id)), nil
}

func NewFrontEnd() *FrontEnd {
	return &FrontEnd{map[uint64]SessionId{}, map[SessionId]uint64{}, 0}
}

func TestGetInitHeap(t *testing.T) {
	init := GetInitHeap(lib.TestId{1})
	expected := []byte(`{"inFlight":{},"inFlightSessionToClient":{},"nextSessionId":0}`)

	if !bytes.Equal(init[0].Diff, expected) {
		t.Error("nope")
	}
}

func TestGetHeapTrace(t *testing.T) {
	diffs := GetHeapTrace(lib.TestId{1}, lib.RunId{0})
	expected := []byte(`{"inFlight":{"0":"0"},"inFlightSessionToClient":{"0":0},"nextSessionId":1}`)
	if !bytes.Equal(diffs[0].Diff, expected) {
		t.Error("nope")
	}
	fe := NewFrontEnd()
	init, err := json.Marshal(fe)
	if err != nil {
		t.Error(err)
	}

	// fmt.Printf("%s\n", string(prettyJson(init)))

	new := applyDiff(init, diffs[0].Diff)
	if !bytes.Equal(expected, new) {
		t.Error("nope2")
	}
	expected2 := []byte(`{"inFlight":{},"inFlightSessionToClient":{},"nextSessionId":1}`)
	new2 := applyDiff(new, diffs[2].Diff)
	if !bytes.Equal(expected2, new2) {
		t.Error("nope2")
	}
}

func TestTraceHeap(t *testing.T) {
	traceHeap(lib.TestId{1}, lib.RunId{0})
}

func TestHeaps(t *testing.T) {
	heaps := Heaps(lib.TestId{1}, lib.RunId{0})
	fmt.Printf("%v", heaps)
}

func TestSequenceDiagrams(t *testing.T) {
	for _, diagram := range SequenceDiagrams(lib.TestId{1}, lib.RunId{0}) {
		fmt.Printf("%s\n", string(diagram))
	}
}
