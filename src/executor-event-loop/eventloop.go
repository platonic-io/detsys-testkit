package executorEL

import (
	"fmt"
	"strings"
	"time"
)

type EventLoop struct {
	// Name     string // hardcoded to "executor" for now
	Admin            *AdminInterface
	CommandTransport *CommandTransport
	Log              []TimestampedLogEntry
	LogicalTime      *LogicalTime
	Executor         *Executor
	SchedulerRef     RemoteRef
}

func NewEventLoop(ai *AdminInterface, ct *CommandTransport, executor *Executor) *EventLoop {
	lt := LogicalTime(0)
	return &EventLoop{
		Admin:            ai,
		CommandTransport: ct,
		Log:              []TimestampedLogEntry{},
		LogicalTime:      &lt,
		Executor:         executor,
		SchedulerRef:     RemoteRef{"scheduler", 0},
	}
}

func (el *EventLoop) AddToLog(logDirection LogDirection, me LocalRef, env Message) {
	tentry := TimestampedLogEntry{
		LogEntry: LogEntry{
			LocalRef:  me,
			RemoteRef: el.SchedulerRef,
			Message:   env,
			Direction: logDirection,
		},
		LogicalTime: *el.LogicalTime,
		Time:        time.Now(),
	}
	el.Log = append(el.Log, tentry)
}

func (el *EventLoop) AddToAdminLog(cmd AdminCommand) {
	// el.AddToLog(LogResumeContinuation, LocalRef{0}, Message{"AdminCommand", []byte(fmt.Sprintf("\"Got command %d\\n\"", cmd))})
}

func (el *EventLoop) toSchedulerEnvelope(me RemoteRef, msg Message, correlationId CorrelationId) Envelope {
	return Envelope{
		Kind:          Response,
		Sender:        me,
		Message:       msg,
		Receiver:      el.SchedulerRef,
		CorrelationId: correlationId,
		LogicalTime:   *el.LogicalTime,
	}
}

func (el *EventLoop) processAdmin(cmd AdminCommand) bool {
	switch cmd.Type {
	case AdminQuit:
		fmt.Printf("Shutting down....\n")
		cmd.Response("ok\n")
		return true
	case AdminDumpLog:
		fmt.Printf("dumping log\n")
		log := make([]string, 0, len(el.Log))
		for _, e := range el.Log {
			log = append(log, e.Serialise())
		}
		toSend := "Log [" + strings.Join(log, ", ") + "]\n"
		cmd.Response(toSend)
		return false
	case AdminResetLog:
		fmt.Printf("resetting log\n")
		el.Log = make([]TimestampedLogEntry, 0)
		cmd.Response("Log reseted\n")
		return false
	default:
		fmt.Printf("Unknown admin command: %#v\n", cmd)
		panic("Unhandled admin command")
	}
}

func (el *EventLoop) Run() {
	adminCommand := el.Admin.Listen()
	commands := el.CommandTransport.Listen()
	for {
		fmt.Print("Looking for command\n")
		select {
		case cmd := <-adminCommand:
			fmt.Printf("Found admin command\n")
			el.LogicalTime.Incr()
			el.AddToAdminLog(cmd)
			quit := el.processAdmin(cmd)
			if quit {
				return
			}
		case envelope := <-commands:
			fmt.Printf("Found message\n")
			me := envelope.Receiver.ToLocal()
			el.LogicalTime.Merge(envelope.LogicalTime)
			el.AddToLog(LogResumeContinuation, me, envelope.Message)
			outgoingMessage := el.Executor.processEnvelope(envelope)
			el.LogicalTime.Incr()
			outgoing := el.toSchedulerEnvelope(envelope.Receiver, outgoingMessage, envelope.CorrelationId)
			el.AddToLog(LogSend, me, outgoingMessage)
			el.CommandTransport.Send(outgoing)
		}
	}
}
