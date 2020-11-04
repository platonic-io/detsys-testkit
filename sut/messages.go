package sut

import (
	"github.com/symbiont-io/detsys/lib"
)

type SessionId struct {
	Id int `json:"id"`
}

func (sid SessionId) MarshalText() ([]byte, error) {
	return []byte(strconv.Itoa(sid.Id)), nil
}

type Read struct {
}

func (_ Read) Request() {}

type Write struct {
	Value int `json:"value"`
}

func (_ Write) Request() {}

type Value struct {
	Value []int `json:"value"`
}

func (_ Value) Response() {}

type Ack struct {
}

func (_ Ack) Response() {}

type InternalRequest struct {
	Id      SessionId   `json:"id"`
	Request lib.Request `json:"request"`
}

func (_ InternalRequest) Message() {}

type InternalResponse struct {
	Id       SessionId    `json:"id"`
	Response lib.Response `json:"response"`
}

func (_ InternalResponse) Message() {}
