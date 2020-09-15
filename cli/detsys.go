package main

import (
	"github.com/symbiont-io/detsys/cli/cmd"
)

func main() {
	// Get a greeting message and print it.
	message := lib.Hello("Gladys")
	fmt.Println(message)
}
