package broadcast

import (
	"log"
	"time"

	"github.com/symbiont-io/detsys/lib"
)

type Node struct {
	Log       string `json:"log"`
	Broadcast bool   `json:"broadcast"`
}

type Broadcast struct {
	Data string `json:"data"`
}

func (b Broadcast) MessageEvent() string {
	return b.Data
}

func NewNode() *Node {
	return &Node{
		Log:       "",
		Broadcast: false,
	}
}

func (n *Node) Receive(_ time.Time, from string, event lib.InEvent) []lib.OutEvent {
	var oevs []lib.OutEvent
	switch ev := event.(type) {
	case *lib.InternalMessage:
		switch msg := (*ev).Message.(type) {
		case Broadcast:
			n.Log = msg.Data
			n.Broadcast = false
		default:
			log.Fatalf("Received unknown message: %#v\n", msg)
			return nil
		}
	default:
		log.Fatalf("Received unknown event: %#v\n", ev)
		return nil
	}
	return oevs
}

func (n *Node) Tick(_ time.Time) []lib.OutEvent {
	var oevs []lib.OutEvent
	if n.Broadcast && n.Log != "" {
		oevs = []lib.OutEvent{
			{
				To: "A",
				Args: &lib.InternalMessage{Broadcast{
					Data: n.Log,
				}},
			},
			{
				To: "B",
				Args: &lib.InternalMessage{Broadcast{
					Data: n.Log,
				}},
			},
			{
				To: "C",
				Args: &lib.InternalMessage{Broadcast{
					Data: n.Log,
				}},
			},
		}
	}
	return oevs
}

func (n *Node) Timer(_ time.Time) []lib.OutEvent {
	return nil
}
