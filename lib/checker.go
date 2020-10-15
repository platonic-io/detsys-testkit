package lib

import (
	"fmt"
	"os"
	"os/exec"
	"strconv"
)

func Check(model string, runId RunId) bool {
	fmt.Printf("Analysing model: `%s` for run %+v\n", model, runId)

	cmd := exec.Command("clj", "-m", "checker.core", model, strconv.Itoa(runId.RunId))

	path, err := os.Getwd()
	if err != nil {
		fmt.Printf("Can't get current working directory:\n%v\n", err)
		return false
	}

	cmd.Dir = path + "/../checker"
	err = cmd.Run()

	if err != nil {
		fmt.Printf("Error occured during analysis:\n%v\n", err)
		return false
	}

	return true
}
