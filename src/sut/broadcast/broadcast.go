package broadcast

import (
	"log"
	"time"

	"github.com/symbiont-io/detsys-testkit/src/lib"
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
	case SimpleDeliv, RetryDeliv, ClassicDeliv:
		broadcast = false
	case RedunDeliv, AckDeliv:
		broadcast = true
	default:
		panic("Unknown round")
	}
	neighbours := make(map[string]bool)
	neighbours[neighbour] = broadcast

	if round == ClassicDeliv {
		// Normally node A isn't considered a neighbour, but in the
		// classic delivery example it seems like node B and C should
		// send messages back to A.
		neighbours["A"] = broadcast
		neighbours[neighbour] = broadcast
	}

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
			if n.Round == AckDeliv {
				ack := lib.OutEvent{
					To:   lib.Singleton(from),
					Args: &lib.InternalMessage{Ack{}},
				}
				oevs = append(oevs, ack)
			} else if n.Round == ClassicDeliv && n.Log != msg.Data {
				// Broadcast back each new message to all
				// neighbours, once.
				var neighbours []lib.Receiver
				for neighbour, _ := range n.Neighbours {
					neighbours = append(neighbours, neighbour)
				}
				oev := lib.OutEvent{
					To: lib.Set(neighbours...),
					Args: &lib.InternalMessage{Broadcast{
						Data: msg.Data,
					}},
				}
				oevs = append(oevs, oev)
			}
			n.Log = msg.Data
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
	return nil
}

func (n *Node) Timer(at time.Time) []lib.OutEvent {
	var oevs []lib.OutEvent
	var neighbours []lib.Receiver
	for neighbour, broadcast := range n.Neighbours {
		if broadcast && n.Log != "" {
			neighbours = append(neighbours, neighbour)
			if n.Round == SimpleDeliv || n.Round == ClassicDeliv {
				n.Neighbours[neighbour] = false
			}
		}
	}
	oev := lib.OutEvent{
		To: lib.Set(neighbours...),
		Args: &lib.InternalMessage{Broadcast{
			Data: n.Log,
		}},
	}
	oevs = append(oevs, oev)

	if n.Round != ClassicDeliv {

		// Renew the timer.
		duration, err := time.ParseDuration("500ms")
		if err != nil {
			panic(err)
		}
		oev2 := lib.OutEvent{
			To: lib.Singleton("scheduler"),
			Args: &lib.Timer{
				Duration: duration,
			},
		}
		oevs = append(oevs, oev2)
	}

	return oevs
}

func (n *Node) Init() []lib.OutEvent {
	var oevs []lib.OutEvent
	duration, err := time.ParseDuration("500ms")
	if err != nil {
		panic(err)
	}
	if n.Round == ClassicDeliv {
		// Only set the timer on node A, otherwise we won't find the
		// counterexample with EFF = 3.
		if n.Log == "Hello world!" {
			oev := lib.OutEvent{
				To: lib.Singleton("scheduler"),
				Args: &lib.Timer{
					Duration: duration,
				},
			}
			oevs = append(oevs, oev)
		}
	} else {
		oev := lib.OutEvent{
			To: lib.Singleton("scheduler"),
			Args: &lib.Timer{
				Duration: duration,
			},
		}
		oevs = append(oevs, oev)
	}
	return oevs
}
