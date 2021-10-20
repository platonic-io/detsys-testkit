package executorEL

import (
	"fmt"
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
		SchedulerRef:     RemoteRef{"/tmp/scheduler", 0},
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
	el.AddToLog(LogResumeContinuation, LocalRef{0}, Message{"AdminCommand", []byte(fmt.Sprintf("Got command %d\n", cmd))})
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

func (el *EventLoop) processAdmin(cmd AdminCommand) (bool, []string) {
	switch cmd {
	case AdminQuit:
		fmt.Printf("Shutting down....\n")
		return true, []string{}
	case AdminDumpLog:
		fmt.Printf("dumping log\n")
		log := make([]string, len(el.Log))
		for _, e := range el.Log {
			log = append(log, e.Serialise())
		}
		return false, log
	case AdminResetLog:
		fmt.Printf("resetting log\n")
		el.Log = make([]TimestampedLogEntry, 0)
		return false, []string{}
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
			quit, output := el.processAdmin(cmd)
			if quit {
				return
			}
			for _, entry := range output {
				el.Admin.Respond(entry)
			}
		case envelope := <-commands:
			fmt.Printf("Found message\n")
			me := envelope.Receiver.ToLocal()
			el.LogicalTime.Merge(envelope.LogicalTime)
			el.AddToLog(LogResumeContinuation, me, envelope.Message)
			outgoingMessage := el.Executor.processEnvelope(envelope)
			outgoing := el.toSchedulerEnvelope(envelope.Receiver, outgoingMessage, envelope.CorrelationId)
			el.AddToLog(LogSend, me, outgoingMessage)
			el.CommandTransport.Send(outgoing)
		}
	}
}
