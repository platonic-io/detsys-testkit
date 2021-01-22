package main

import (
	"context"
	"log"
	"net/http"
	"os"
	"os/signal"

	"github.com/symbiont-io/detsys-testkit/src/executor"
	"github.com/symbiont-io/detsys-testkit/src/lib"
	"github.com/symbiont-io/detsys-testkit/src/sut/register"
)

func constructor(name string) lib.Reactor {
	switch name {
	case "frontend":
		return sut.NewFrontEnd()
	case "register":
		return sut.NewRegister()
	default:
		panic(name)
	}
}

func main() {
	testId, err := lib.ParseTestId(os.Args[1])
	if err != nil {
		panic(err)
	}

	var srv http.Server

	idleConnsClosed := make(chan struct{})
	go func() {
		sigint := make(chan os.Signal, 1)
		signal.Notify(sigint, os.Interrupt)
		<-sigint

		// We received an interrupt signal, shut down.
		if err := srv.Shutdown(context.Background()); err != nil {
			// Error from closing listeners, or context timeout:
			log.Printf("HTTP server Shutdown: %v", err)
		}
		close(idleConnsClosed)
	}()

	marshaler := sut.NewMarshaler()
	eventLog := lib.EventLogEmitter{
		Component: "Register cmd",
		TestId:    &testId,
		RunId:     nil,
	}
	// TODO(stevan): nil below should be topology?
	executor.DeployRaw(&srv, testId, eventLog, nil, marshaler, constructor)

	<-idleConnsClosed
}
