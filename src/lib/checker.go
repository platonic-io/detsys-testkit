package lib

import (
	"fmt"
	"os"
	"os/exec"
	"strconv"
)

func Check(model string, testId TestId, runId RunId) bool {
	cmd := exec.Command("./checker", model,
		strconv.Itoa(testId.TestId),
		strconv.Itoa(runId.RunId))

	path, err := os.Getwd()
	if err != nil {
		fmt.Printf("Can't get current working directory:\n%v\n", err)
		return false
	}

	cmd.Dir = path + "/../checker/target/"

	out, err := cmd.CombinedOutput()

	if err != nil {
		fmt.Printf("Error occured during analysis:\n%s\n%s\n", err, string(out))
		return false
	}

	return true
}
