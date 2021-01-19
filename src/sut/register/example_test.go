package sut

import (
	"log"
	"net/http"
	"testing"
	"time"

	"github.com/symbiont-io/detsys-testkit/src/executor"
	"github.com/symbiont-io/detsys-testkit/src/lib"
)

func createTopology(newFrontEnd func() lib.Reactor) map[string]lib.Reactor {
	return map[string]lib.Reactor{
		"frontend":  newFrontEnd(),
		"register1": NewRegister(),
		"register2": NewRegister(),
	}
}

func once(newFrontEnd func() lib.Reactor, testId lib.TestId, t *testing.T) (lib.RunId, bool) {
	topology := createTopology(newFrontEnd)
	marshaler := NewMarshaler()
	var srv http.Server
	lib.Setup(func() {
		executor.Deploy(&srv, topology, marshaler)
	})
	qs := lib.LoadTest(testId)
	lib.SetSeed(lib.Seed{4})
	log.Printf("Loaded test of size: %d\n", qs.QueueSize)
	lib.Register(testId)
	runId := lib.CreateRun(testId)
	lib.Run()
	log.Printf("Finished run id: %d\n", runId.RunId)
	lib.Teardown(&srv)
	model := "list-append"
	log.Printf("Analysing model %s for %+v and %+v\n", model, testId, runId)
	result := lib.Check(model, testId, runId)
	return runId, result
}

func testRegisterWithFrontEnd(newFrontEnd func() lib.Reactor, tickFrequency float64, t *testing.T) {
	agenda := []lib.ScheduledEvent{
		lib.ScheduledEvent{
			At:   time.Unix(0, 0).UTC(),
			From: "client:0",
			To:   "frontend",
			Event: lib.ClientRequest{
				Id:      0,
				Request: Write{1},
			},
		},
		lib.ScheduledEvent{
			At:   time.Unix(0, 0).UTC().Add(time.Duration(10) * time.Second),
			From: "client:0",
			To:   "frontend",
			Event: lib.ClientRequest{
				Id:      0,
				Request: Read{},
			},
		},
	}

	testId := lib.GenerateTestFromTopologyAndAgenda(createTopology(newFrontEnd), agenda)

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
		lib.SetTickFrequency(tickFrequency)
		log.Printf("Injecting faults: %#v\n", faults)
		runId, result := once(newFrontEnd, testId, t)
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

func TestRegister1(t *testing.T) {
	testRegisterWithFrontEnd(func() lib.Reactor { return NewFrontEnd() }, 5000.0, t)
}

func TestRegister2(t *testing.T) {
	testRegisterWithFrontEnd(func() lib.Reactor { return NewFrontEnd2() }, 1000.0, t)
}

func TestRegister3(t *testing.T) {
	testRegisterWithFrontEnd(func() lib.Reactor { return NewFrontEnd3() }, 1000.0, t)
}

func TestRegister4(t *testing.T) {
	testRegisterWithFrontEnd(func() lib.Reactor { return NewFrontEnd4() }, 10*1000*1000.0, t)
}
