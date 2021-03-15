package broadcast

import (
	"log"
	"net/http"
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

func many(round Round, t *testing.T) {
	tickFrequency := 100000000000.0 // Make ticks infrequent.

	testId := lib.GenerateTest("broadcast")

	var faults []lib.Fault
	failSpec := lib.FailSpec{
		EFF:     5,
		Crashes: 1,
		EOT:     10,
	}
	for {
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

func TestSimpleDelivRound1(t *testing.T) {
	many(SimpleDeliv, t)
}

func TestRetryDelivRound2(t *testing.T) {
	many(RetryDeliv, t)
}

func TestRedunDelivRound3(t *testing.T) {
	many(RedunDeliv, t)
}

func TestAckDelivRound4(t *testing.T) {
	many(AckDeliv, t)
}

func TestClassicDelivRound5(t *testing.T) {
	many(ClassicDeliv, t)
}
