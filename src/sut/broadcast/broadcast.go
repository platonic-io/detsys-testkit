package broadcast

import (
	"log"
	"time"

	"github.com/symbiont-io/detsys/lib"
)

type Round int

const (
	// Round 1: Naive broadcast
	//
	// The broadcasting node sends a single message is sent to each neighbour. No
	// redundancy in neither space nor time.
	SimpleDeliv Round = iota

	// Round 2: Retrying broadcast
	//
	// The broadcasting node resends the message indefinitely to its neighbours. In
	// this example we got redundancy in time, but not in space.
	RetryDeliv

	// Round 3: Redundant broadcast
	//
	// All nodes that got the message will resends the message indefinitely to its
	// neighbours. In this example we got redundancy in time and in space.
	RedunDeliv

	// Round 4: Finite redundant broadcast
	//
	// All nodes that got the message will resends the message to its neighbours
	// until they get an acknowledgement. In this example is basically a more
	// efficient version of the previous round (RedunDeliv).
	AckDeliv

	// Round 5: "Classic" broadcast
	//
	// Send the message once to all neighbours, neighbours will also send it
	// once to all its neighbours. This example is redundant in space but
	// not in time.
	ClassicDeliv
)

type Node struct {
	// The log of the received contents stored on "disk" of the node.
	Log string `json:"log"`

	// Map of neighbour nodes. The bool keeps track of whether we should
	// broadcast to that node or not.
	Neighbours map[string]bool `json:"neighbours`

	// The round or version of the implementation.
	Round Round `json:"round"`
}

type Broadcast struct {
	Data string `json:"data"`
}

func (_ Broadcast) MessageEvent() string {
	return "Broadcast"
}

type Ack struct{}

func (_ Ack) MessageEvent() string {
	return "Ack"
}

func NewNodeA(round Round) *Node {
	return &Node{
		Log: "Hello world!",
		Neighbours: map[string]bool{
			"B": true,
			"C": true,
		},
		Round: round,
	}
}

func NewNode(round Round, neighbour string) *Node {
	var broadcast bool
	switch round {
	case SimpleDeliv, RetryDeliv:
		broadcast = false
	case RedunDeliv, AckDeliv, ClassicDeliv:
		broadcast = true
	default:
		panic("Unknown round")
	}
	neighbours := make(map[string]bool)
	neighbours[neighbour] = broadcast
	return &Node{
		Log:        "",
		Neighbours: neighbours,
		Round:      round,
	}
}

func (n *Node) Receive(_ time.Time, from string, event lib.InEvent) []lib.OutEvent {
	var oevs []lib.OutEvent
	switch ev := event.(type) {
	case *lib.InternalMessage:
		switch msg := (*ev).Message.(type) {
		case Broadcast:
			n.Log = msg.Data
			if n.Round == AckDeliv {
				ack := lib.OutEvent{
					To:   from,
					Args: &lib.InternalMessage{Ack{}},
				}
				oevs = append(oevs, ack)
			}
		case Ack:
			if n.Round != AckDeliv {
				panic("Got unexpected Ack message")
			}
			n.Neighbours[from] = false
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
	for neighbour, broadcast := range n.Neighbours {
		if broadcast && n.Log != "" {
			oev := lib.OutEvent{
				To: neighbour,
				Args: &lib.InternalMessage{Broadcast{
					Data: n.Log,
				}},
			}
			oevs = append(oevs, oev)

			if n.Round == SimpleDeliv || n.Round == ClassicDeliv {
				n.Neighbours[neighbour] = false
			}
		}
	}
	return oevs
}

func (n *Node) Timer(_ time.Time) []lib.OutEvent {
	return nil
}
