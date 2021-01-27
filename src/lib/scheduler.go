package lib

import (
	"encoding/json"
	"fmt"
	"strconv"
	"time"
)

type RunId struct {
	RunId int
}

func (runId *RunId) UnmarshalJSON(b []byte) error {
	var i int
	if err := json.Unmarshal(b, &i); err != nil {
		return err
	}
	runId.RunId = i
	return nil
}

func (runId RunId) MarshalJSON() ([]byte, error) {
	return []byte(strconv.Itoa(runId.RunId)), nil
}

type QueueSize struct {
	QueueSize int `json:"queue-size"`
}

type Seed int

func LoadTest(testId TestId) QueueSize {
	var queueSize QueueSize
	PostParse("load-test!", struct {
		TestId TestId `json:"test-id"`
	}{testId}, &queueSize)
	return queueSize
}

func RegisterExecutor(executorId string, components []string) {
	Post("register-executor!", struct {
		ExecutorId string   `json:"executor-id"`
		Components []string `json:"components"`
	}{
		ExecutorId: executorId,
		Components: components,
	})
}

type SchedulerFault struct {
	Kind string `json:"kind"`
	From string `json:"from"`
	To   string `json:"to"`
	At   int    `json:"at"` // should be time.Time?
}

func toSchedulerFaults(faults Faults) []SchedulerFault {
	schedulerFaults := make([]SchedulerFault, 0, len(faults.Faults))
	for _, fault := range faults.Faults {
		var schedulerFault SchedulerFault
		switch ev := fault.Args.(type) {
		case Omission:
			schedulerFault.Kind = fault.Kind
			schedulerFault.From = ev.From
			schedulerFault.To = ev.To
			schedulerFault.At = ev.At // convert?
		case Crash:
			schedulerFault.Kind = fault.Kind
			schedulerFault.From = ev.From
			schedulerFault.To = ""    // Not used.
			schedulerFault.At = ev.At // convert?
		default:
			panic(fmt.Sprintf("Unknown fault type: %#v\n", fault))
		}
		schedulerFaults = append(schedulerFaults, schedulerFault)

	}
	return schedulerFaults
}

type CreateRunEvent struct {
	Seed          Seed
	Faults        Faults
	TickFrequency float64
	MinTimeNs     time.Duration
	MaxTimeNs     time.Duration
}

func CreateRun(testId TestId, event CreateRunEvent) RunId {
	var runId struct {
		RunId RunId `json:"run-id"`
	}
	PostParse("create-run-event!", struct {
		TestId        TestId           `json:"test-id"`
		Seed          Seed             `json:"seed"`
		Faults        []SchedulerFault `json:"faults"`
		TickFrequency float64          `json:"tick-frequency"`
		MinTimeNs     time.Duration    `json:"min-time-ns"`
		MaxTimeNs     time.Duration    `json:"max-time-ns"`
	}{testId, event.Seed, toSchedulerFaults(event.Faults), event.TickFrequency, event.MinTimeNs, event.MaxTimeNs}, &runId)
	return runId.RunId

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

func Step() json.RawMessage {
	var result json.RawMessage
	PostParse("step!", struct{}{}, &result)
	return result
}

func componentsFromDeployment(testId TestId) ([]string, error) {
	query := fmt.Sprintf(`SELECT component
                              FROM deployment
                              WHERE test_id = %d`, testId.TestId)

	db := OpenDB()
	defer db.Close()

	rows, err := db.Query(query)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var components []string
	type Column struct {
		Component string
	}
	for rows.Next() {
		column := Column{}
		err := rows.Scan(&column.Component)
		if err != nil {
			return nil, err
		}
		components = append(components, column.Component)
	}
	return components, nil
}

func Register(testId TestId) {
	// TODO(stevan): Make executorUrl part of topology/deployment.
	const executorUrl string = "http://localhost:3001/api/v1/"

	components, err := componentsFromDeployment(testId)
	if err != nil {
		panic(err)
	}

	RegisterExecutor(executorUrl, components)
}
