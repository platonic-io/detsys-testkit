package lib

import (
	"log"
	"os"
	"os/exec"
	"strconv"
	"strings"
)

func GenerateTest() TestId {
	cmd := exec.Command("./generator.sh")

	path, err := os.Getwd()
	if err != nil {
		log.Fatal(err)
	}

	cmd.Dir = path + "/../generator/"

	out, err := cmd.Output()

	if err != nil {
		log.Fatal(err)
	}

	i, err := strconv.Atoi(strings.TrimRight(string(out), "\n"))

	if err != nil {
		log.Fatal(err)
	}

	return TestId{i}
}
