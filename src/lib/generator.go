package lib

import (
	"log"
	"os/exec"
	"strconv"
	"strings"
)

func GenerateTest(test string) TestId {

	cmd := exec.Command("detsys-generator", test)
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
