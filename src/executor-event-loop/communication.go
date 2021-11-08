package executorEL

import (
	"bufio"
	"encoding/json"
	"net"
	"os"
)

type CommandTransport struct {
	Started       bool
	Incomming     string
	ListenChannel chan Envelope
}

func NewCommandTransport(input string) *CommandTransport {
	com := make(chan Envelope)
	return &CommandTransport{
		Started:       false,
		Incomming:     input,
		ListenChannel: com,
	}
}

func (ct *CommandTransport) Send(env Envelope) {
	j, err := json.Marshal(env)
	if err != nil {
		panic(err)
	}

	c, err := net.Dial("unix", "/tmp/"+env.Receiver.Address+".sock")
	if err != nil {
		panic(err)
	}
	defer c.Close()

	c.Write(j)
}

func (ct *CommandTransport) findIncoming() {
	if err := os.RemoveAll(ct.Incomming); err != nil {
		panic(err)
	}

	l, err := net.Listen("unix", ct.Incomming)
	if err != nil {
		panic(err)
	}
	defer l.Close()
	for {
		conn, err := l.Accept()
		if err != nil {
			panic(err)
		}
		buf := bufio.NewScanner(conn)
		defer conn.Close()

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
