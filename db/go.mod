module github.com/symbiont-io/detsys/detsys-db

go 1.15

replace github.com/symbiont-io/detsys/lib => ../lib

require (
	github.com/mattn/go-sqlite3 v1.14.5
	github.com/rubenv/sql-migrate v0.0.0-20200616145509-8d140a17f351
	github.com/symbiont-io/detsys/lib v0.0.0-00010101000000-000000000000
)
