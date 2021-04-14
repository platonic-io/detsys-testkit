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

func once(round Round, testId lib.TestId, runEvent lib.CreateRunEvent, t *testing.T) (lib.RunId, lib.LTLResult) {
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
	log.Printf("Finished run id: %d %d\n", testId.TestId, runId.RunId)
	lib.Teardown(&srv)
	log.Printf("Checking\n")
	result := lib.LtlChecker(testId, runId, "<> @B'.log = `\"Hello world!\"`")
	return runId, result
}

func many(round Round, expectedRuns int, t *testing.T, expectedFaults []lib.Fault) {
	tickFrequency := 100000000000.0 // Make ticks infrequent.

	// TODO(stevan): GenerateTest should be parametrised by round also.
	// Currently one test_info.deployment is shared between all rounds,
	// which is wrong and makes the debugger display the wrong initial
	// state.
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
		if !result.Result {
			fmt.Printf("%+v and %+v doesn't pass analysis\n%s\n", testId, runId, result.Reason)
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
	many(RetryDeliv, 3, t,
		[]lib.Fault{
			lib.Fault{
				Kind: "crash",
				Args: lib.Crash{From: "A", At: 3}},
			lib.Fault{
				Kind: "omission",
				Args: lib.Omission{From: "A", To: "B", At: 3}},
		})
}

// TODO(stevan): It would be nice to certify that this example doesn't have a
// bug for a higher EFF. If we increase the EFF and decrease the timer duration
// a fault where "B" is crashed is introduced causing the checker to fail. We
// should probably add a pre-condition that ensures that "B" doesn't crash.
func TestRedunDelivRound3(t *testing.T) {
	many(RedunDeliv, 3, t, []lib.Fault{})
}

// TODO(stevan): Same remark as above for round 3.
func TestAckDelivRound4(t *testing.T) {
	many(AckDeliv, 3, t, []lib.Fault{})
}

// TODO(stevan): The counterexample according to the paper is: `[omission(A, B,
// 1), omission(C, A, 2), omission(C, B, 2)]`. Note that they use "sent logical
// time" while we use "received logical time". It's not clear to me if dropping
// both messages from "A" to "B" and "C" at send time 1 is valid, if so why
// isn't that the counterexample (which is what we find, modulo the unnecessary
// crash).
func TestClassicDelivRound5(t *testing.T) {
	many(ClassicDeliv, 3, t,
		[]lib.Fault{
			lib.Fault{
				Kind: "crash",
				Args: lib.Crash{From: "A", At: 3}},
			lib.Fault{
				Kind: "omission",
				Args: lib.Omission{From: "A", To: "B", At: 2}},
			lib.Fault{
				Kind: "omission",
				Args: lib.Omission{From: "A", To: "C", At: 3}},
		})
}
