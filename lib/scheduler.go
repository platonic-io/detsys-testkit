package lib

import (
	"fmt"
	"strconv"
)

type TestId struct {
	TestId int `json:"test-id"`
}

func ParseTestId(s string) (TestId, error) {
	i, err := strconv.Atoi(s)
	if err != nil {
		return TestId{-1}, err
	}
	return TestId{i}, nil
}

type RunId struct {
	RunId int `json:"run-id"`
}

type QueueSize struct {
	QueueSize int `json:"queue-size"`
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

func CreateRun(testId TestId) RunId {
	var runId RunId
	PostParse("create-run!", testId, &runId)
	return runId
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
	// TODO(stevan): Make executorUrl and nodes part of topology.
	const executorUrl string = "http://localhost:3001"
	RegisterExecutor(executorUrl, []string{"node1", "node2"})
	runId := CreateRun(testId)
	Run()
	fmt.Printf("Finished run id: %d\n", runId.RunId)
	return runId
}
