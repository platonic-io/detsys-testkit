package executor

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"

	"github.com/symbiont-io/detsys/lib"
)

func jsonError(s string) string {
	return fmt.Sprintf("{\"error\":\"%s\"}", s)
}

func handler(topology map[string]lib.Reactor, un lib.Unmarshaler, m lib.Marshaler) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json; charset=utf-8")
		if r.Method != "POST" {
			http.Error(w, jsonError("Method is not supported."),
				http.StatusNotFound)
			return
		}
		log.Println("Handling request")
		// if err := decodeJSONBody(w, r, &msg); err != nil {
		// 	var mr *malformedRequest
		// 	if errors.As(err, &mr) {
		// 		log.Printf("%+v\n", err)
		// 		http.Error(w, jsonError(mr.msg), mr.status)
		// 	} else {
		// 		log.Printf("%+v\n", err)
		// 		http.Error(w,
		// 			jsonError(http.StatusText(
		// 				http.StatusInternalServerError)),
		// 			http.StatusInternalServerError)
		// 	}
		// 	return
		// }
		r.Body = http.MaxBytesReader(w, r.Body, 1048576)
		// XXX: Inefficient, reuse parts of decodeJSONBody?
		body, err := ioutil.ReadAll(r.Body)
		if err != nil {
			panic(err)
		}
		var sev lib.ScheduledEvent
		if err := lib.UnmarshalScheduledEvent(un, body, &sev); err != nil {
			panic(err)
		}
		log.Printf("Handling event: %+v", sev)
		oevs := topology[sev.To].Receive(sev.At, sev.From, sev.Event)
		bs := lib.MarshalUnscheduledEvents(m, sev.To, oevs)
		fmt.Fprint(w, string(bs))
	}
}

func Register(topology map[string]lib.Reactor) {
	// TODO(stevan): Make executorUrl part of topology.
	const executorUrl string = "http://localhost:3001/api/v1/event"

	components := make([]string, 0, len(topology))
	for component, _ := range topology {
		components = append(components, component)
	}
	lib.RegisterExecutor(executorUrl, components)
}

func Deploy(topology map[string]lib.Reactor, un lib.Unmarshaler, m lib.Marshaler) {
	log.Printf("Deploying topology: %+v\n", topology)
	mux := http.NewServeMux()
	mux.HandleFunc("/api/v1/event", handler(topology, un, m))
	err := http.ListenAndServe(":3001", mux)
	panic(err)
}
