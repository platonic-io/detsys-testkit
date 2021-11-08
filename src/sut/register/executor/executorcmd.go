package main

import (
	"fmt"

	"github.com/symbiont-io/detsys-testkit/src/executorEL"
	"github.com/symbiont-io/detsys-testkit/src/lib"
	"github.com/symbiont-io/detsys-testkit/src/sut/register"
)

func main() {

	fmt.Printf("Starting up executor\n")
	adminI := executorEL.NewAdmin("/tmp/executor-admin.sock")
	fmt.Printf("Created admin\n")
	commandT, err := executorEL.NewCommandTransport("/tmp/executor.sock")
	if err != nil {
		panic(err)
	}
	fmt.Printf("Created command transport\n")
	topology := lib.NewTopology(
		lib.Item{"frontend", sut.NewFrontEnd()},
		lib.Item{"register1", sut.NewRegister()},
		lib.Item{"register2", sut.NewRegister()},
	)
	exe := executorEL.NewExecutor(topology, sut.NewMarshaler())
	el := executorEL.NewEventLoop(adminI, commandT, exe)

	el.Run()
	fmt.Printf("Finished running\n")
}
