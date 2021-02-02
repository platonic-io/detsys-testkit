package lib

import (
	"fmt"
	"sort"
)

// ---------------------------------------------------------------------
// Custom type for `Topology`, since this type is a `map` we need to make
// sure we use it in a deterministic way.

type Topology struct {
	topology map[string]Reactor
}

type Item struct {
	Name    string
	Reactor Reactor
}

func NewTopology(init ...Item) Topology {
	topology := make(map[string]Reactor)
	for _, m := range init {
		topology[m.Name] = m.Reactor
	}
	return Topology{
		topology: topology,
	}
}

func (t Topology) Len() int {
	return len(t.topology)
}

func (t Topology) Reactors() []string {
	reactors := make([]string, 0, len(t.topology))
	for r, _ := range t.topology {
		reactors = append(reactors, r)
	}
	sort.Strings(reactors)
	return reactors
}

func (t Topology) Reactor(reactorName string) Reactor {
	reactor, ok := t.topology[reactorName]
	if !ok {
		panic(fmt.Sprintf("The topology doesn't have reactor '%s'", reactorName))
	}
	return reactor
}

func (t Topology) Insert(reactorName string, reactor Reactor) {
	t.topology[reactorName] = reactor
}
