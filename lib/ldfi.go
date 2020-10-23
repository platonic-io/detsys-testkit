package lib

import (
	"encoding/json"
	"log"
	"os"
	"os/exec"
	"strconv"
	"strings"
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
		log.Fatal("Impossible: unknown kind")
	}
	return nil
}

func Ldfi(testId TestId, runIds []RunId, fail FailSpec) Faults {
	var runIdsArg []string
	for _, runId := range runIds {
		runIdsArg = append(runIdsArg, strconv.Itoa(runId.RunId))
	}
	cmd := exec.Command("./ldfi.py",
		"--json",
		"--eff", strconv.Itoa(fail.EFF),
		"--crashes", strconv.Itoa(fail.Crashes),
		"--test-id", strconv.Itoa(testId.TestId),
		"--run-ids", strings.Join(runIdsArg, " "))

	path, err := os.Getwd()
	if err != nil {
		log.Fatal(err)
	}

	cmd.Dir = path + "/../ldfi/"

	out, err := cmd.Output()

	if err != nil {
		log.Fatal(err)
	}

	var faults Faults
	err = json.Unmarshal(out, &faults)

	if err != nil {
		log.Fatal(err)
	}

	return faults
}
