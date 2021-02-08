module github.com/symbiont-io/detsys-testkit/src/logger

go 1.15

replace github.com/symbiont-io/detsys-testkit/src/lib => ../lib

replace github.com/symbiont-io/detsys-testkit/src/executor => ../executor

require (
	github.com/mattn/go-sqlite3 v1.14.5
	github.com/symbiont-io/detsys-testkit/src/executor v0.0.0-00010101000000-000000000000
	github.com/symbiont-io/detsys-testkit/src/lib v0.0.0-00010101000000-000000000000
)
