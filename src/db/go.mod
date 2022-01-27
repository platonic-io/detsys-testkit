module github.com/symbiont-io/detsys-testkit/src/db

go 1.15

replace github.com/symbiont-io/detsys-testkit/src/lib => ../lib

require (
	github.com/mattn/go-sqlite3 v1.14.6
	github.com/rubenv/sql-migrate v1.0.0
	github.com/symbiont-io/detsys-testkit/src/lib v0.0.0-00010101000000-000000000000
	github.com/ziutek/mymysql v1.5.4 // indirect
)
