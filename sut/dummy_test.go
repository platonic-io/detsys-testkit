package sut

import (
	"fmt"
	"testing"

	"github.com/symbiont-io/detsys/executor"
	"github.com/symbiont-io/detsys/lib"
)

func TestDummy(t *testing.T) {
	frontEnd := NewFrontEnd()
	topology := map[string]lib.Reactor{
		"frontend":  frontEnd,
		"register1": NewRegister(),
		"register2": NewRegister(),
	}
	lib.Reset()
	testId := lib.GenerateTest()
	qs := lib.LoadTest(testId)
	fmt.Printf("Loaded test of size: %d\n", qs.QueueSize)
	lib.Setup(func() {
		executor.Deploy(topology, frontEnd, frontEnd)
	})
	executor.Register(topology)
	runId := lib.CreateRun(testId)
	lib.Run()
	fmt.Printf("Finished run id: %d\n", runId.RunId)
	lib.Teardown()
	result := lib.Check("list-append", testId, runId)
	if !result {
		t.Errorf("Test-run %d doesn't pass analysis", runId)
	}

}
