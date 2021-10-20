package executorEL

import (
	"encoding/json"
	jsonpatch "github.com/evanphx/json-patch"
)

func dumpHeapJson(component interface{}) []byte {
	heap, err := json.Marshal(component)
	if err != nil {
		panic(err)
	}
	return heap
}

func jsonDiff(original []byte, modified []byte) []byte {
	diff, err := jsonpatch.CreateMergePatch(original, modified)
	if err != nil {
		panic(err)
	}
	return diff
}
