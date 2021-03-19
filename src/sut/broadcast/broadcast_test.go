package broadcast

import (
	"fmt"
	"log"
	"net/http"
	"reflect"
	"testing"
	"time"

	"github.com/symbiont-io/detsys-testkit/src/executor"
	"github.com/symbiont-io/detsys-testkit/src/lib"
)

func once(round Round, testId lib.TestId, runEvent lib.CreateRunEvent, t *testing.T) (lib.RunId, bool) {
	topology := lib.NewTopology(
		lib.Item{"A", NewNodeA(round)},
		lib.Item{"B", NewNode(round, "C")},
		lib.Item{"C", NewNode(round, "B")},
	)
	marshaler := NewMarshaler()
	var srv http.Server
	lib.Setup(func() {
		executor.Deploy(&srv, topology, marshaler)
	})
	qs := lib.LoadTest(testId)
	log.Printf("Loaded test of size: %d\n", qs.QueueSize)
	lib.Register(testId)
	log.Printf("Registered executor")
	runId := lib.CreateRun(testId, runEvent)
	log.Printf("Created run id: %v", runId)
	lib.Run()
	log.Printf("Finished run id: %d\n", runId.RunId)
	lib.Teardown(&srv)
	log.Printf("Checking\n")
	nodeB := topology.Reactor("B").(*Node)
	result := nodeB.Log == "Hello world!"
	return runId, result
}

func many(round Round, expectedRuns int, t *testing.T, expectedFaults []lib.Fault) {
	tickFrequency := 100000000000.0 // Make ticks infrequent.

	testId := lib.GenerateTest("broadcast")

	var faults []lib.Fault
	failSpec := lib.FailSpec{
		// NOTE: EFF is set to 2 in the paper. There's a mismatch here,
		// because in the paper two nodes can send messages at the same
		// time, we can't. And we also got timers which increase the
		// logical clock, which means we sometimes need a higher EFF in
		// order to find the problem.
		EFF:     3,
		Crashes: 1,
		EOT:     10,
	}
	neededRuns := 0
	for {
		neededRuns++
		lib.Reset()
		maxTime := time.Duration(5) * time.Second
		runEvent := lib.CreateRunEvent{
			Seed:          lib.Seed(1),
			Faults:        lib.Faults{faults},
			TickFrequency: tickFrequency,
			MaxTimeNs:     maxTime,
			MinTimeNs:     0,
		}
		runId, result := once(round, testId, runEvent, t)
		if !result {
			fmt.Printf("%+v and %+v doesn't pass analysis\n", testId, runId)
			if !reflect.DeepEqual(faults, expectedFaults) {
				t.Errorf("Expected faults:\n%#v, but got faults:\n%#v\n",
					expectedFaults, faults)
			}
			break
		}
		faults = lib.Ldfi(testId, nil, failSpec).Faults
		if len(faults) == 0 {
			break
		}
	}
	if !reflect.DeepEqual(faults, []lib.Fault{}) {
		if neededRuns != expectedRuns {
			t.Errorf("Expected to find counterexample in %d runs, but it took %d runs",
				expectedRuns, neededRuns)
		}
	} else {
		if neededRuns != expectedRuns {
			t.Errorf("Expected to find no counterexamples in %d runs, but it took %d runs",
				expectedRuns, neededRuns)
		}
	}
}

func TestSimpleDelivRound1(t *testing.T) {
	many(SimpleDeliv, 2, t,
		[]lib.Fault{
			lib.Fault{
				Kind: "omission",
				Args: lib.Omission{From: "A", To: "B", At: 3}}})
}

func TestRetryDelivRound2(t *testing.T) {
	many(RetryDeliv, 2, t,
		[]lib.Fault{
			lib.Fault{
				Kind: "crash",
				Args: lib.Crash{From: "A", At: 5}},
			lib.Fault{
				Kind: "omission",
				Args: lib.Omission{From: "A", To: "B", At: 3}},
		})
}

func TestRedunDelivRound3(t *testing.T) {
	many(RedunDeliv, 5, t, []lib.Fault{})
}

func TestAckDelivRound4(t *testing.T) {
	many(AckDeliv, 5, t, []lib.Fault{})
}

func TestClassicDelivRound5(t *testing.T) {
	many(ClassicDeliv, 2, t, []lib.Fault{})
}
