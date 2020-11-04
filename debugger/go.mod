module github.com/symbiont-io/detsys/debugger

go 1.15

replace github.com/symbiont-io/detsys/lib => ../lib

require (
	github.com/evanphx/json-patch v4.9.0+incompatible
	github.com/evanphx/json-patch/v5 v5.1.0 // indirect
	github.com/mattn/go-sqlite3 v1.14.4
	github.com/symbiont-io/detsys/lib v0.0.0-00010101000000-000000000000
)
