module github.com/symbiont-io/detsys-testkit/src/executorEL

go 1.15

replace github.com/symbiont-io/detsys-testkit/src/lib => ../lib

require (
	github.com/evanphx/json-patch v4.9.0+incompatible
	github.com/pkg/errors v0.8.1 // indirect
	github.com/symbiont-io/detsys-testkit/src/lib v0.0.0-20211029070032-cb4608e5c898
)
