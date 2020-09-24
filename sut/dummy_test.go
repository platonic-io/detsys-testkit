package sut

import (
	"testing"

	"github.com/symbiont-io/detsys/executor"
	"github.com/symbiont-io/detsys/lib"
)

func TestDummy(t *testing.T) {
	lib.Setup(func() {
		executor.Deploy(map[string]lib.Reactor{
			"node1": &Node{},
			"node2": &Node{}})
	})
	lib.Reset()
	// testId := lib.Generate()
	testId := lib.TestId{1}
	runId := lib.Execute(testId)
	lib.Teardown()
	// lib.Check
}
