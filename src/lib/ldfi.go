package lib

import (
	"encoding/json"
	"log"
	"os/exec"
	"strconv"
	"strings"
	"time"
)

type FailSpec struct {
	EFF     int // End (time) of finite failures.
	Crashes int // Max amount of node (permanent) crashes.
	EOT     int // End of time, for the test.
}

type Fault struct {
	Kind string
	Args FaultArgs
}

type FaultArgs interface{ FaultArgs() }

type Omission struct {
	From string
	To   string
	At   int
}

func (_ Omission) FaultArgs() {}

type Crash struct {
	From string
	At   int
}

func (_ Crash) FaultArgs() {}

type Faults = struct {
	Faults []Fault `json:"faults"`
}

func (f *Fault) UnmarshalJSON(bs []byte) error {
	var s map[string]interface{}
	if err := json.Unmarshal(bs, &s); err != nil {
		return err
	}
	switch strings.ToLower(s["kind"].(string)) {
	case "omission":
		*f = Fault{
			Kind: "omission",
			Args: Omission{
				From: s["from"].(string),
				To:   s["to"].(string),
				At:   int(s["at"].(float64)),
			},
		}
	case "crash":
		*f = Fault{
			Kind: "crash",
			Args: Crash{
				From: s["from"].(string),
				At:   int(s["at"].(float64)),
			},
		}
	default:
		log.Panicf("Unknown kind: %+v\n", s)
	}
	return nil
}

func Ldfi(testId TestId, runIds []RunId, fail FailSpec) Faults {
	start := time.Now()
	args := []string{
		"--json",
		"--eff", strconv.Itoa(fail.EFF),
		"--crashes", strconv.Itoa(fail.Crashes),
		"--test-id", strconv.Itoa(testId.TestId),
		"--run-ids",
	}
	for _, runId := range runIds {
		args = append(args, strconv.Itoa(runId.RunId))
	}
	cmd := exec.Command("detsys-ldfi", args...)

	out, err := cmd.CombinedOutput()

	if err != nil {
		log.Panicf("%s\n%s\n", err, out)
	}

	var result struct {
		Faults     []Fault                `json:"faults"`
		Statistics map[string]interface{} `json:"statistics"`
	}
	err = json.Unmarshal(out, &result)

	if err != nil {
		log.Panic(err)
	}

	elapsed := time.Since(start)
	log.Printf("ldfi time: %v\n", elapsed)
	log.Printf("z3 statistics: %+v\n", result.Statistics)

	return Faults{result.Faults}
}
