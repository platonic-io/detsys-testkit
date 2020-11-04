package lib

import (
	"fmt"
	"log"
	"strconv"
)

func ParseTestId(s string) (TestId, error) {
	i, err := strconv.Atoi(s)
	if err != nil {
		return TestId{}, err
	}
	return TestId{i}, nil
}

type RunId struct {
	RunId int `json:"run-id"`
}

type QueueSize struct {
	QueueSize int `json:"queue-size"`
}

type Seed struct {
	Seed int `json:"new-seed"`
}

func LoadTest(testId TestId) QueueSize {
	var queueSize QueueSize
	PostParse("load-test!", testId, &queueSize)
	return queueSize
}

func RegisterExecutor(executorId string, components []string) {
	Post("register-executor", struct {
		ExecutorId string   `json:"executor-id"`
		Components []string `json:"components"`
	}{
		ExecutorId: executorId,
		Components: components,
	})
}

func SetSeed(seed Seed) {
	Post("set-seed!", seed)
}

func CreateRun(testId TestId) RunId {
	var runId RunId
	PostParse("create-run!", testId, &runId)
	return runId
}

func InjectFaults(faults Faults) {
	type SchedulerFault struct {
		Kind string `json:"kind"`
		From string `json:"from"`
		To   string `json:"to"`
		At   int `json:"at"` // should be time.Time?
	}
	schedulerFaults := make([]SchedulerFault, 0, len(faults.Faults))
	for _, fault := range faults.Faults {
		var schedulerFault SchedulerFault
		switch ev := fault.Args.(type) {
		case Omission:
			//assert fault.Kind?
			schedulerFault.Kind = fault.Kind;
			schedulerFault.From = ev.From;
			schedulerFault.To = ev.To;
			schedulerFault.At = ev.At; // convert?
		default:
			log.Panic("Unknown fault type: %#v\n", fault)
		}
		schedulerFaults = append(schedulerFaults, schedulerFault)

	}
	Post("inject-faults!", struct{
		Faults []SchedulerFault `json:"faults"`
	}{schedulerFaults})
}

func Run() {
	Post("run!", struct{}{})
}

func Status() map[string]interface{} {
	var status map[string]interface{}
	PostParse("status", struct{}{}, &status)
	return status
}

func Reset() {
	Post("reset", struct{}{})
}

func Execute(testId TestId) RunId {
	qs := LoadTest(testId)
	fmt.Printf("Loaded test of size: %d\n", qs.QueueSize)
	runId := CreateRun(testId)
	Run()
	fmt.Printf("Finished run id: %d\n", runId.RunId)
	return runId
}
