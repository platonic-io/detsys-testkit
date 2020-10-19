package lib

import (
	"fmt"
	"os"
	"os/exec"
	"strconv"
)

func Check(model string, testId TestId, runId RunId) bool {
	fmt.Printf("Analysing model: `%s` for run %+v of test %+v\n", model, runId, testId)

	cmd := exec.Command("clj", "-m", "checker.core", model,
		strconv.Itoa(testId.TestId),
		strconv.Itoa(runId.RunId))

	path, err := os.Getwd()
	if err != nil {
		fmt.Printf("Can't get current working directory:\n%v\n", err)
		return false
	}

	cmd.Dir = path + "/../checker/"

	out, err := cmd.CombinedOutput()

	if err != nil {
		fmt.Printf("Error occured during analysis:\n%s\n", string(out))
		return false
	}

	return true
}
