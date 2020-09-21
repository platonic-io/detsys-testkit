package sut

import (
	"testing"
	"time"

	"github.com/symbiont-io/detsys/executor"
	"github.com/symbiont-io/detsys/lib"
)

var quit chan struct{}

func setup() {
	quit = make(chan struct{})
	go loop()
}

func teardown() {
	close(quit)
}

func loop() {
	for {
		select {
		case <-quit:
			return
		default:
			executor.Deploy(map[string]lib.Reactor{
				"node1": &Node{},
				"node2": &Node{}})
		}
	}
}

func TestDummy(t *testing.T) {
	setup()
	time.Sleep(3 * time.Second)
	teardown()
	// lib.Execute(1)
	// lib.Check
}
