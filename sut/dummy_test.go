package sut

import (
	"testing"

	"github.com/symbiont-io/detsys/executor"
	"github.com/symbiont-io/detsys/lib"
)

func TestDummy(t *testing.T) {
	executor.Deploy(map[string]lib.Reactor{"node1": &Node{}, "node2": &Node{}})
	// lib.Execute(1)
	// lib.Check
}
