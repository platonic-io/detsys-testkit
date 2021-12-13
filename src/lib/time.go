package lib

import (
	"time"
)

type TimePico time.Time

func (t *TimePico) UnmarshalJSON(data []byte) error {
	// Ignore null, like in the main JSON package.
	if string(data) == "null" {
		return nil
	}
	// Fractional seconds are handled implicitly by Parse.
	it, err := time.Parse(`"2006-01-02T15:04:05.999999999999Z"`, string(data))
	if err != nil {
		*t = TimePico(it)
	}
	return err
}
