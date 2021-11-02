package executorEL

/*
   These seems to not be synchronised, so we could get problems correlating input/output
*/

import (
	"bufio"
	"fmt"
	"os"
)

type AdminCommand int

const (
	AdminQuit = iota
	AdminDumpLog
	AdminResetLog
	AdminUnknown
)

// panic galore if we don't have it
func parseCommandFromString(command string) AdminCommand {
	v, ok := map[string]AdminCommand{
		"AdminQuit":     AdminQuit,
		"AdminDumpLog":  AdminDumpLog,
		"AdminResetLog": AdminResetLog,
	}[command]
	if !ok {
		return AdminUnknown
	} else {
		return v
	}
}

type AdminInterface struct {
	Started       bool
	Incoming      string
	Outgoing      string
	ListenChannel chan AdminCommand
}

func NewAdmin(input string, output string) (*AdminInterface, *error) {
	err := createPipe(input)
	if err != nil {
		return nil, err
	}

	err = createPipe(output)
	if err != nil {
		return nil, err
	}

	com := make(chan AdminCommand)
	return &AdminInterface{
		Started:       false,
		Incoming:      input,
		Outgoing:      output,
		ListenChannel: com,
	}, nil
}

func (a *AdminInterface) findIncoming() {
	for {
		file, err := openPipe(a.Incoming, os.O_RDONLY)
		if err != nil {
			panic(err)
		}
		buf := bufio.NewScanner(file)
		for buf.Scan() {
			line := buf.Text()
			if line == "" {
				continue
			}
			command := parseCommandFromString(line)
			if command != AdminUnknown {
				a.ListenChannel <- command
			} else {
				fmt.Printf("Unknown command: %s\n", line)
			}
		}
	}
}

func (a *AdminInterface) Listen() <-chan AdminCommand {
	if !a.Started {
		go a.findIncoming()
		a.Started = true
	}
	return a.ListenChannel
}

func (a *AdminInterface) Respond(line string) {
	if !a.Started {
		panic("Can't send on admin interface before you Listen()")
	}
	writePipe(a.Outgoing, line)
}
