package main

import (
	"github.com/symbiont-io/detsys-testkit/src/lib"
	"github.com/symbiont-io/detsys-testkit/src/logger"
)

func main() {
	var (
		reactor   lib.Reactor   = logger.NewLogger()
		marshaler lib.Marshaler = logger.NewMarshaler()
	)
	oevs := reactor.Init()
	if oevs != nil {
		panic("init returned messages")
	}
	logger.DeployReadOnlyPipe("detsys-logger", reactor, marshaler)
}
