package main

import (
	"github.com/symbiont-io/detsys-testkit/src/cli/cmd"
)

var version = "unknown"

func main() {
	cmd.Execute(version)
}
