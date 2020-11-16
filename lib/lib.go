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
}

type Marshaler interface {
	UnmarshalRequest(request string, input json.RawMessage, output *Request) error
	UnmarshalMessage(message string, input json.RawMessage, output *Message) error
	MarshalEvent(_ Args) string
}

// ---------------------------------------------------------------------
// Types

type ScheduledEvent struct {
	At    time.Time `json:"at"`
	From  string    `json:"from"`
	To    string    `json:"to"`
	Event InEvent   `json:"event"`
}

type InEvent interface{ InEvent() }

type ClientRequest struct {
	Id      uint64
	Request Request
}

type Request interface{ Request() }

func (_ ClientRequest) InEvent() {}

type InternalMessage struct {
	Message Message `json:"message"`
}

func (im InternalMessage) MarshalJSON() ([]byte, error) {
	return json.Marshal(im.Message)
}

type Message interface{ Message() }

func (_ InternalMessage) InEvent() {}

type Args interface{ Args() }

type ClientResponse struct {
	Id       uint64   `json:"id"`
	Response Response `json:"response"`
}

type Response interface{ Response() }

type Timer struct {
	Duration time.Duration `json:"duration"`
}

func (_ ClientResponse) Args()  {}
func (_ InternalMessage) Args() {}
func (_ Timer) Args()           {}

type OutEvent struct {
	To   string
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
