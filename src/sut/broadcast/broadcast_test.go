package broadcast

import (
	"log"
	"net/http"
	"testing"
	"time"

	"github.com/symbiont-io/detsys-testkit/src/executor"
	"github.com/symbiont-io/detsys-testkit/src/lib"
)

func once(round Round, testId lib.TestId, t *testing.T) (lib.RunId, bool) {
	topology := map[string]lib.Reactor{
		"A": NewNodeA(round),
		"B": NewNode(round, "C"),
		"C": NewNode(round, "B"),
	}
	marshaler := NewMarshaler()
	eventLog := lib.EventLogEmitter{
		Component: "Broadcast test",
		TestId:    &testId,
		RunId:     nil,
	}
	var srv http.Server
	lib.Setup(func() {
		executor.Deploy(&srv, testId, eventLog, topology, marshaler)
	})
	qs := lib.LoadTest(testId)
	log.Printf("Loaded test of size: %d\n", qs.QueueSize)
	lib.Register(testId)
	log.Printf("Registered executor")
	runId := lib.CreateRun(testId)
	eventLog.RunId = &runId
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
	tickFrequency := 100000000000.0 // Make ticks infrequent.

	testId := lib.GenerateTest("broadcast")

	var runIds []lib.RunId
	var faults []lib.Fault
	failSpec := lib.FailSpec{
		EFF:     5,
		Crashes: 1,
		EOT:     10,
	}
	for {
		lib.Reset()
		lib.InjectFaults(lib.Faults{faults})
		lib.SetTickFrequency(tickFrequency)
		maxTime, err := time.ParseDuration("5s")
		if err != nil {
			panic(err)
		}
		lib.SetMaxTimeNs(maxTime)
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
