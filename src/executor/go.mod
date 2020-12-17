module github.com/symbiont-io/detsys-testkit/src/executor

go 1.15

replace github.com/symbiont-io/detsys-testkit/src/lib => ../lib

require (
	github.com/evanphx/json-patch v4.9.0+incompatible
	github.com/mattn/go-sqlite3 v1.14.4
	go.uber.org/zap v1.16.0
)
