package logger

import (
	"log"
	"net/http"
	"testing"

	"github.com/symbiont-io/detsys-testkit/src/executor"
	"github.com/symbiont-io/detsys-testkit/src/lib"
)

// XXX: frontend

func once(testId lib.TestId, runEvent lib.CreateRunEvent, t *testing.T) (lib.RunId, bool) {
	topology := lib.NewTopology(
		lib.Item{"Logger", NewLogger()},
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
	model := "list-append"
	log.Printf("Analysing model %s for %+v and %+v\n", model, testId, runId)
	result := lib.Check(model, testId, runId)
	return runId, result
}

func many(iterations int, t *testing.T) {
	tickFrequency := 100000000000.0 // Make ticks infrequent.

	testId := lib.GenerateTest("logger")

	for i := 0; i < iterations; i++ {
		lib.Reset()
		runEvent := lib.CreateRunEvent{
			Seed:          lib.Seed(i), // TODO(stevan): more randomness?
			Faults:        lib.Faults{[]lib.Fault{}},
			TickFrequency: tickFrequency,
			MaxTimeNs:     0,
			MinTimeNs:     0,
		}
		runId, result := once(testId, runEvent, t)
		if !result {
			t.Errorf("%+v and %+v doesn't pass analysis", testId, runId)
			break
		}
	}
}

func TestLogger(t *testing.T) {
	many(1, t)
}
