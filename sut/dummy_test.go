package sut

import (
	"log"
	"testing"

	"github.com/symbiont-io/detsys/executor"
	"github.com/symbiont-io/detsys/lib"
)

func once(testId lib.TestId, topology map[string]lib.Reactor, t *testing.T) lib.RunId {
	frontEnd := NewFrontEnd()
	lib.Setup(func() {
		executor.Deploy(topology,
			frontEnd, // TODO(stevan): can we get rid of this?
			frontEnd)
	})
	qs := lib.LoadTest(testId)
	log.Printf("Loaded test of size: %d\n", qs.QueueSize)
	executor.Register(topology)
	runId := lib.CreateRun(testId)
	lib.Run()
	log.Printf("Finished run id: %d\n", runId.RunId)
	lib.Teardown()
	result := lib.Check("list-append", testId, runId)
	if !result {
		t.Errorf("Test-run %d doesn't pass analysis", runId)
	}
	return runId
}

func TestDummy(t *testing.T) {
	frontEnd := NewFrontEnd()
	topology := map[string]lib.Reactor{
		"frontend":  frontEnd,
		"register1": NewRegister(),
		"register2": NewRegister(),
	}
	testId := lib.GenerateTest()

	var runIds []lib.RunId
	var faults []lib.Fault
	failSpec := lib.FailSpec{
		EFF:     0,
		Crashes: 0,
		EOT:     0,
	}
	for {
		lib.Reset()
		lib.InjectFaults(lib.Faults{faults})
		runId := once(testId, topology, t)
		runIds = append(runIds, runId)
		faults = lib.Ldfi(testId, runIds, failSpec).Faults
		if len(faults) == 0 {
			break
		}
	}

}
