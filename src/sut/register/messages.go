package sut

import (
	"strconv"

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

func (_ Read) RequestEvent() string { return "read" }

type Write struct {
	Value int `json:"value"`
}

func (_ Write) RequestEvent() string { return "write" }

type Value struct {
	Value []int `json:"value"`
}

func (_ Value) ResponseEvent() string { return "value" }

type Ack struct {
}

func (_ Ack) ResponseEvent() string { return "ack" }

type InternalRequest struct {
	Id      SessionId   `json:"id"`
	Request lib.Request `json:"request"`
}

func (im InternalRequest) MessageEvent() string {
	return im.Request.RequestEvent()
}

type InternalResponse struct {
	Id       SessionId    `json:"id"`
	Response lib.Response `json:"response"`
}

func (ir InternalResponse) MessageEvent() string {
	return ir.Response.ResponseEvent()
}
