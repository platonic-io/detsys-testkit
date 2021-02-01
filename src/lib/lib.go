package lib

import (
	"context"
	"encoding/json"
	"net/http"
	"time"
)

// ---------------------------------------------------------------------
// Interfaces

type Reactor interface {
	Receive(at time.Time, from string, event InEvent) []OutEvent
	Tick(at time.Time) []OutEvent
	Timer(at time.Time) []OutEvent
	Init() []OutEvent
}

type Marshaler interface {
	UnmarshalRequest(request string, input json.RawMessage, output *Request) error
	UnmarshalMessage(message string, input json.RawMessage, output *Message) error
}

// ---------------------------------------------------------------------
// Types

type MetaInfo struct {
	TestId      TestId `json:"test-id"`
	RunId       RunId  `json:"run-id"`
	LogicalTime int    `json:"logical-time"`
}

type ScheduledEvent struct {
	At    time.Time
	From  string
	To    string
	Event InEvent
	Meta  MetaInfo
}

type InEvent interface{ InEvent() }

type ClientRequest struct {
	Id      uint64
	Request Request
}

type Request interface {
	RequestEvent() string
}

func (_ ClientRequest) InEvent() {}

type InternalMessage struct {
	Message Message `json:"message"`
}

func (im InternalMessage) MessageEvent() string {
	return im.Message.MessageEvent()
}

func (im InternalMessage) MarshalJSON() ([]byte, error) {
	return json.Marshal(im.Message)
}

type Message interface {
	MessageEvent() string
}

func (_ InternalMessage) InEvent() {}

type Args interface{ Args() }

type ClientResponse struct {
	Id       uint64   `json:"id"`
	Response Response `json:"response"`
}

func (c ClientResponse) ResponseEvent() string {
	return c.Response.ResponseEvent()
}

type Response interface {
	ResponseEvent() string
}

type Timer struct {
	Duration time.Duration `json:"duration"`
}

func (_ ClientResponse) Args()  {}
func (_ InternalMessage) Args() {}
func (_ Timer) Args()           {}

type Receiver = string

func Singleton(to Receiver) []Receiver {
	return Set(to)
}

func Set(to ...Receiver) []Receiver {
	return to
}

type OutEvent struct {
	To   []Receiver // Assumed to be a set.
	Args Args
}

// ---------------------------------------------------------------------

var quit chan struct{}

func Setup(f func()) {
	quit = make(chan struct{})
	go loop(f)
}

func Teardown(srv *http.Server) {
	close(quit)
	if err := srv.Shutdown(context.Background()); err != nil {
		panic(err)
	}
}

func loop(f func()) {
	for {
		select {
		case <-quit:
			return
		default:
			f()
		}
	}
}
