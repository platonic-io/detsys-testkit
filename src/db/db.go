package main

import (
	"fmt"
	_ "github.com/mattn/go-sqlite3"
	"github.com/rubenv/sql-migrate"
	"os"
	"path/filepath"

	"github.com/symbiont-io/detsys-testkit/lib"
)

var version = "unknown"

func help() {
	fmt.Printf(`
Usage:
  detsys-db [up|down]
Flags:
  -v, --version   version for detsys-db
`)
	os.Exit(1)
}

func main() {
	if len(os.Args) != 2 {
		help()
	}

	var dir migrate.MigrationDirection
	switch os.Args[1] {
	case "up":
		dir = migrate.Up
	case "down":
		dir = migrate.Down
	case "-v":
		fmt.Println(version)
		os.Exit(0)
	case "--version":
		fmt.Println(version)
		os.Exit(0)
	default:
		help()
	}

	path, err := os.Executable()
	if err != nil {
		panic(err)
	}

	migrations := &migrate.FileMigrationSource{
		Dir: fmt.Sprintf("%s/../migrations", filepath.Dir(path)),
	}

	db := lib.OpenDB()

	n, err := migrate.Exec(db, "sqlite3", migrations, dir)
	if err != nil {
		panic(fmt.Sprintf("%s\n%s", err, migrations))
	}
	fmt.Printf("Applied %d migrations!\n", n)
}
