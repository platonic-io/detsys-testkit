package lib

import (
	"encoding/json"
	"log"
	"os"
	"os/exec"
	"strconv"
	"time"
)

type LTLResult struct {
	Result bool   `json:"result"`
	Reason string `json:"reason"`
}

func LtlChecker(testId TestId, runId RunId, formula string) LTLResult {
	start := time.Now()
	cmd := exec.Command("detsys-ltl", "check", "--testId", strconv.Itoa(testId.TestId), "--runId", strconv.Itoa(runId.RunId), "--formula", formula)
	cmd.Stderr = os.Stderr

	out, err := cmd.Output()

	if err != nil {
		panic(err)
	}
	var result LTLResult

	err = json.Unmarshal(out, &result)
	elapsed := time.Since(start)
	log.Printf("LTL Checker time: %v\n", elapsed)

	return result
}
