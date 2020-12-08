module github.com/symbiont-io/detsys/sut/register

go 1.15

replace github.com/symbiont-io/detsys/executor => ../../executor

replace github.com/symbiont-io/detsys/lib => ../../lib

require (
	github.com/symbiont-io/detsys/executor v0.0.0-00010101000000-000000000000
	github.com/symbiont-io/detsys/lib v0.0.0-00010101000000-000000000000
)
