module github.com/symbiont-io/detsys/detsys

go 1.13

replace github.com/symbiont-io/detsys => ../

replace github.com/symbiont-io/detsys/lib => ../lib

require (
	github.com/spf13/cobra v1.0.0
	github.com/symbiont-io/detsys/lib v0.0.0-00010101000000-000000000000
)
