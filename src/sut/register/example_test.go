package sut

import (
	"log"
	"net/http"
	"testing"
	"time"

	"github.com/symbiont-io/detsys-testkit/src/executor"
	"github.com/symbiont-io/detsys-testkit/src/lib"
)

func createTopology(newFrontEnd func() lib.Reactor) lib.Topology {
	return lib.NewTopology(
		lib.Item{"frontend", newFrontEnd()},
		lib.Item{"register1", NewRegister()},
		lib.Item{"register2", NewRegister()},
	)
}

func once(newFrontEnd func() lib.Reactor, testId lib.TestId, runEvent lib.CreateRunEvent, t *testing.T) (lib.RunId, bool) {
	topology := createTopology(newFrontEnd)
	marshaler := NewMarshaler()
	var srv http.Server
	lib.Setup(func() {
		executor.Deploy(&srv, topology, marshaler)
	})
	qs := lib.LoadTest(testId)
	log.Printf("Loaded test of size: %d\n", qs.QueueSize)
	lib.Register(testId)
	runId := lib.CreateRun(testId, runEvent)
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

	var faults []lib.Fault
	failSpec := lib.FailSpec{
		EFF:     10,
		Crashes: 0,
		EOT:     0,
	}
	for {
		lib.Reset()
		runEvent := lib.CreateRunEvent{
			Seed:          lib.Seed(4),
			Faults:        lib.Faults{faults},
			TickFrequency: tickFrequency,
			MinTimeNs:     0,
			MaxTimeNs:     0,
		}
		log.Printf("Injecting faults: %#v\n", faults)
		runId, result := once(newFrontEnd, testId, runEvent, t)
		if !result {
			t.Errorf("%+v and %+v doesn't pass analysis", testId, runId)
			t.Errorf("faults: %#v\n", faults)
			break
		}
		faults = lib.Ldfi(testId, nil, failSpec).Faults
		if len(faults) == 0 {
			break
		}
	}
}

// In the first version of the frontend the implementation forwards the request
// from the client to both registers and replies to the client with the first
// response it gets from either register.
//
// Given that our test case is that a client first writes and then reads a
// value, we expect to find a counterexample where we first update one of the
// registers and then read from the other giving us a stale value.
func TestRegister1(t *testing.T) {
	testRegisterWithFrontEnd(func() lib.Reactor { return NewFrontEnd() }, 5000.0, t)
}

// In the next version of the frontend we try to avoid the above problem by
// waiting for both registers to respond before we respond to the client.

// The implementation is still broken though, because the frontend is happy as
// soon as it gets two responses from the registers -- it doesn't check that
// these responses are actually from the two separate registers.

// So given the same test case as before, we expect a counterexample where the
// frontend forwards the write, one of the registers fails to respond, the
// frontend retries, the same register fails to respond again, but now the
// frontend has received two acknowledgments and tells the client that the
// write has been persisted. The client then tries to read the value and gets
// a stale value again.
func TestRegister2(t *testing.T) {
	testRegisterWithFrontEnd(func() lib.Reactor { return NewFrontEnd2() }, 1000.0, t)
}

func TestRegister3(t *testing.T) {
	testRegisterWithFrontEnd(func() lib.Reactor { return NewFrontEnd3() }, 1000.0, t)
}

func TestRegister4(t *testing.T) {
	testRegisterWithFrontEnd(func() lib.Reactor { return NewFrontEnd4() }, 10*1000*1000.0, t)
}
