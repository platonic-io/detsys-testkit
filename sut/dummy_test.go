package sut

import (
	"testing"

	"github.com/symbiont-io/detsys/executor"
	"github.com/symbiont-io/detsys/lib"
)

func TestDummy(t *testing.T) {
	lib.Setup(func() {
		executor.Deploy(map[string]lib.Reactor{
			"frontend":  NewFrontEnd(),
			"register1": NewRegister(),
			"register2": NewRegister(),
		})
	})
	lib.Reset()
	// testId := lib.Generate()
	testId := lib.TestId{1}
	lib.Execute(testId)
	lib.Teardown()
	// lib.Check
}
