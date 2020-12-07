#### Pseudo code for the discrete-event simulator

```
Model = record
  { peers      : Map NodeName Component/Node
  , partitions : List Partition // See extensions below.
  }

1.  m     := initialModel     // Model of the system.
2.  el    := emptyEventList   // Priority queue, sorted by time.
3.  time  := 0                // Simulation time.
4.  seed  := initialSeed      // Used for randomness.
5.  stats := emptyStats       // Statistics about the system.

    // Environment events include client requests and faults.
5.  (es, seed) := generateEnvironmentEvents m seed

    // These events need to be tagged with a receiving processes, i.e. which
    // component gets the client request or suffers the injected fault, and a
    // time of the event.
6.  (es, seed) := tagEventsWithProcessAndTime es seed
7.  el.push(es) // Push events onto the event list.

8.  while el not empty and simulation time not at end
9.    e           := el.pop()      // Take the first event, in chronological order.
10.   time        := time + e.time // Advance the simulation clock.

      // Process the event using the tagged component. The components are part
      // of the model and it needs to be possible to suspend (e.g. if they get
      // stuck waiting for a network message) and resume them (when the message
      // event arrives). Also updates the statistical counters.
11.   (es, m, stats) := process e m stats

      // While processing an event the components might emit other events, e.g.
      // send messages to other components.
12.   (es, seed) := tagEventsWithTime es seed
13.   el.push(es)
14.   checkInvariants m
15. end while
16. computeEstimatesAndGenerateReport stats
```
