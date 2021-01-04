module github.com/symbiont-io/detsys-detsys/src/sut/broadcast

go 1.15

replace github.com/symbiont-io/detsys-testkit/src/executor => ../../executor

replace github.com/symbiont-io/detsys-testkit/src/lib => ../../lib

require (
	github.com/symbiont-io/detsys-testkit/src/executor v0.0.0-00010101000000-000000000000
	github.com/symbiont-io/detsys-testkit/src/lib v0.0.0-00010101000000-000000000000
)
