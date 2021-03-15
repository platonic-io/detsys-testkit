package lib

import (
	"encoding/json"
	"log"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"time"
)

type FailSpec struct {
	EFF         int // End (time) of finite failures.
	Crashes     int // Max amount of node (permanent) crashes.
	EOT         int // End of time, for the test.
	LimitFaults int // Have a limit of how many faults may be generated, 0 is no limit
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

func Ldfi(testId TestId, failedRunIds []RunId, fail FailSpec) Faults {
	start := time.Now()
	args := []string{
		"--endOfFiniteFailures", strconv.Itoa(fail.EFF),
		"--maxCrashes", strconv.Itoa(fail.Crashes),
		"--testId", strconv.Itoa(testId.TestId),
		"--endOfTime", strconv.Itoa(0), // not used
	}
	if fail.LimitFaults > 0 {
		args = append(args, "--limitNumberOfFaults")
		args = append(args, strconv.Itoa(fail.LimitFaults))
	}
	for _, r := range failedRunIds {
		args = append(args, "--failedRunId")
		args = append(args, strconv.Itoa(r.RunId))
	}
	cmd := exec.Command("detsys-ldfi", args...)
	cmd.Stderr = os.Stderr

	out, err := cmd.Output()

	if err != nil {
		log.Panicf("%s\n%s\n", err, out)
	}

	var result struct {
		Faults []Fault `json:"faults"`
	}
	err = json.Unmarshal(out, &result)

	if err != nil {
		log.Panic(err)
	}

	elapsed := time.Since(start)
	log.Printf("ldfi time: %v\n", elapsed)

	return Faults{result.Faults}
}
