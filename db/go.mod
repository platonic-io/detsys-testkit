module github.com/symbiont-io/detsys/detsys-db

go 1.15

replace github.com/symbiont-io/detsys/lib => ../lib

require (
	github.com/gobuffalo/packr v1.30.1
	github.com/gobuffalo/packr/v2 v2.8.0 // indirect
	github.com/mattn/go-sqlite3 v1.12.0
	github.com/rubenv/sql-migrate v0.0.0-20200616145509-8d140a17f351
	github.com/symbiont-io/detsys/lib v0.0.0-00010101000000-000000000000
)
