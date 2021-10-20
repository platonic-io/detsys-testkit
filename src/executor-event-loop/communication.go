package executorEL

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
)

type CommandTransport struct {
	Started       bool
	Incomming     string
	ListenChannel chan Envelope
}

func NewCommandTransport(input string) (*CommandTransport, *error) {
	err := createPipe(input)
	if err != nil {
		return nil, err
	}

	com := make(chan Envelope)
	return &CommandTransport{
		Started:       false,
		Incomming:     input,
		ListenChannel: com,
	}, nil
}

func (ct *CommandTransport) Send(env Envelope) {
	j, err := json.Marshal(env)
	if err != nil {
		panic(err)
	}

	writePipe(env.Receiver.Address, string(j)+"\n")
}

func (ct *CommandTransport) findIncoming() {
	for {

		file, err := openPipe(ct.Incomming, os.O_RDONLY)
		if err != nil {
			panic(err)
		}

		buf := bufio.NewScanner(file)
		for buf.Scan() {
			line := buf.Text() // is this correct?
			if line == "" {
				continue
			}
			var envelope *Envelope
			err := json.Unmarshal([]byte(line), &envelope)
			if err != nil {
				panic(err)
			}

			ct.ListenChannel <- *envelope
		}
	}
}

func (ct *CommandTransport) Listen() <-chan Envelope {
	if !ct.Started {
		go ct.findIncoming()
		ct.Started = true
	}

	return ct.ListenChannel
}
