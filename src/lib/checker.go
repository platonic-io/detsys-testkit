package lib

import (
	"fmt"
	"os/exec"
	"strconv"
)

func Check(model string, testId TestId, runId RunId) bool {
	cmd := exec.Command("detsys-checker", model,
		strconv.Itoa(testId.TestId),
		strconv.Itoa(runId.RunId))

	out, err := cmd.CombinedOutput()

	if err != nil {
		fmt.Printf("Error occured during analysis:\n%s\n%s\n", err, string(out))
		return false
	}

	return true
}
