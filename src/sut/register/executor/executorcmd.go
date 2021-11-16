package main

import (
	"fmt"

	"github.com/symbiont-io/detsys-testkit/src/executorEL"
	"github.com/symbiont-io/detsys-testkit/src/lib"
	"github.com/symbiont-io/detsys-testkit/src/sut/register"
)

func constructor(name string) lib.Reactor {
	switch name {
	case "frontend":
		return sut.NewFrontEnd4()
	default:
		return sut.NewRegister()
	}
}

func main() {
	fmt.Printf("Starting up executor\n")
	adminI := executorEL.NewAdmin("/tmp/executor-admin.sock")
	fmt.Printf("Created admin\n")
	commandT := executorEL.NewCommandTransport("/tmp/executor.sock")
	fmt.Printf("Created command transport\n")
	peers := []string{"frontend", "register1", "register2"}
	exe := executorEL.NewExecutor(peers, constructor, sut.NewMarshaler())
	el := executorEL.NewEventLoop(adminI, commandT, exe)

	el.Run()
	fmt.Printf("Finished running\n")
}
