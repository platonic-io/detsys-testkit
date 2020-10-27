package sut

import (
	"log"
	"net/http"
	"testing"

	"github.com/symbiont-io/detsys/executor"
	"github.com/symbiont-io/detsys/lib"
)

func once(testId lib.TestId, t *testing.T) (lib.RunId, bool) {
	topology := map[string]lib.Reactor{
		"frontend":  NewFrontEnd2(),
		"register1": NewRegister(),
		"register2": NewRegister(),
	}
	marshaler := NewMarshaler()
	var srv http.Server
	lib.Setup(func() {
		executor.Deploy(&srv, topology, marshaler)
	})
	qs := lib.LoadTest(testId)
	lib.SetSeed(lib.Seed{4})
	log.Printf("Loaded test of size: %d\n", qs.QueueSize)
	executor.Register(topology)
	runId := lib.CreateRun(testId)
	lib.Run()
	log.Printf("Finished run id: %d\n", runId.RunId)
	lib.Teardown(&srv)
	model := "list-append"
	log.Printf("Analysing model %s for %+v and %+v\n", model, testId, runId)
	result := lib.Check(model, testId, runId)
	return runId, result
}

func TestRegister(t *testing.T) {
	testId := lib.GenerateTest()

	var runIds []lib.RunId
	var faults []lib.Fault
	failSpec := lib.FailSpec{
		EFF:     10,
		Crashes: 0,
		EOT:     0,
	}
	for {
		lib.Reset()
		lib.InjectFaults(lib.Faults{faults})
		log.Printf("Injecting faults: %#v\n", faults)
		runId, result := once(testId, t)
		if !result {
			t.Errorf("%+v and %+v doesn't pass analysis", testId, runId)
			t.Errorf("faults: %#v\n", faults)
			break
		}
		runIds = append(runIds, runId)
		faults = lib.Ldfi(testId, runIds, failSpec).Faults
		if len(faults) == 0 {
			break
		}
	}

}
