module github.com/symbiont-io/detsys-testkit/src/sut/register

go 1.15

replace github.com/symbiont-io/detsys-testkit/src/executor => ../../executor

replace github.com/symbiont-io/detsys-testkit/src/executorEL => ../../executor-event-loop

replace github.com/symbiont-io/detsys-testkit/src/lib => ../../lib

require (
	github.com/symbiont-io/detsys-testkit/src/executor v0.0.0-00010101000000-000000000000
	github.com/symbiont-io/detsys-testkit/src/executorEL v0.0.0-00010101000000-000000000000 // indirect
	github.com/symbiont-io/detsys-testkit/src/lib v0.0.0-20211029070032-cb4608e5c898
	go.uber.org/zap v1.16.0 // indirect
)
