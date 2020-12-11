package broadcast

import (
	"log"
	"net/http"
	"testing"

	"github.com/symbiont-io/detsys/executor"
	"github.com/symbiont-io/detsys/lib"
)

func once(round Round, testId lib.TestId, t *testing.T) (lib.RunId, bool) {
	topology := map[string]lib.Reactor{
		"A": NewNodeA(round),
		"B": NewNode(round, "C"),
		"C": NewNode(round, "B"),
	}
	marshaler := NewMarshaler()
	var srv http.Server
	lib.Setup(func() {
		executor.Deploy(&srv, testId, topology, marshaler)
	})
	qs := lib.LoadTest(testId)
	lib.SetMinTimeNs(10*10 ^ 9)
	log.Printf("Loaded test of size: %d\n", qs.QueueSize)
	executor.Register(topology)
	log.Printf("Registered executor")
	runId := lib.CreateRun(testId)
	log.Printf("Created run id: %v", runId)
	lib.Run()
	log.Printf("Finished run id: %d\n", runId.RunId)
	lib.Teardown(&srv)
	log.Printf("Checking\n")
	nodeB := topology["B"].(*Node)
	result := nodeB.Log == "Hello world!"
	return runId, result
}

func many(round Round, t *testing.T) {
	tickFrequency := 1000.0 // One tick per second.

	testId := lib.GenerateTest("broadcast")

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
		runId, result := once(round, testId, t)
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

func TestSimpleDeliv(t *testing.T) {
	many(SimpleDeliv, t)
}

func TestRetryDeliv(t *testing.T) {
	many(RetryDeliv, t)
}

func TestRedunDeliv(t *testing.T) {
	many(RedunDeliv, t)
}

func TestAckDeliv(t *testing.T) {
	many(AckDeliv, t)
}

func TestClassicDeliv(t *testing.T) {
	many(ClassicDeliv, t)
}
