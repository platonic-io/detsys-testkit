package executorEL

/*
   These seems to not be synchronised, so we could get problems correlating input/output
*/

import (
	"bufio"
	"fmt"
	"net"
	"os"
)

type AdminCommandType int

const (
	AdminQuit = iota
	AdminDumpLog
	AdminResetLog
	AdminResetReactors
	AdminUnknown
)

type AdminCommand struct {
	Type     AdminCommandType
	Response func(string)
}

// panic galore if we don't have it
func parseCommandFromString(command string) AdminCommandType {
	v, ok := map[string]AdminCommandType{
		"AdminQuit":          AdminQuit,
		"AdminDumpLog":       AdminDumpLog,
		"AdminResetLog":      AdminResetLog,
		"AdminResetReactors": AdminResetReactors,
	}[command]
	if !ok {
		return AdminUnknown
	} else {
		return v
	}
}

type AdminInterface struct {
	Started       bool
	Domain        string
	ListenChannel chan AdminCommand
}

func NewAdmin(domain string) *AdminInterface {
	com := make(chan AdminCommand)
	return &AdminInterface{
		Started:       false,
		Domain:        domain,
		ListenChannel: com,
	}
}

func (a *AdminInterface) findIncoming() {
	if err := os.RemoveAll(a.Domain); err != nil {
		panic(err)
	}
	l, err := net.Listen("unix", a.Domain)
	if err != nil {
		panic(err)
	}
	defer l.Close()
	for {
		conn, err := l.Accept()
		if err != nil {
			panic(err)
		}
		defer conn.Close()
		buf := bufio.NewScanner(conn)
		for buf.Scan() {
			line := buf.Text()
			if line == "" {
				continue
			}
			command := parseCommandFromString(line)
			if command != AdminUnknown {
				a.ListenChannel <- AdminCommand{command, func(line string) {
					conn.Write([]byte(line))
					conn.Close()
				}}
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
