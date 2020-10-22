package lib

import (
	"encoding/json"
	"testing"
)

func TestLdfiFaultJsonUnmarshal(t *testing.T) {
	bs := []byte(`{"kind":"omission", "from":"a", "to":"b", "at":0}`)
	var fault Fault
	err := json.Unmarshal(bs, &fault)

	if err != nil {
		t.Error(err)
	}
	o := fault.Args.(Omission)
	if o.From != "a" || o.To != "b" || o.At != 0 {
		t.Errorf("%+v", o)
	}

}

func TestLdfiFaultsJsonUnmarshal(t *testing.T) {
	bs := []byte(`{"faults":[{"kind":"omission", "from":"a", "to":"b", "at":0},
                                 {"kind":"crash", "from":"c", "at":1}]}`)
	var faults Faults
	err := json.Unmarshal(bs, &faults)

	if err != nil {
		t.Error(err)
	}

	o := faults.Faults[0].Args.(Omission)
	if o.From != "a" || o.To != "b" || o.At != 0 {
		t.Errorf("%+v", o)
	}
	c := faults.Faults[1].Args.(Crash)
	if c.From != "c" || c.At != 1 {
		t.Errorf("%+v", c)
	}
}
