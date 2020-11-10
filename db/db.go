package main

import (
	"fmt"
	"github.com/gobuffalo/packr"
	_ "github.com/mattn/go-sqlite3"
	"github.com/rubenv/sql-migrate"
	"os"

	"github.com/symbiont-io/detsys/lib"
)

func help() {
	fmt.Printf("Usage: db [up|down]\n")
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
	default:
		help()
	}

	//migrations := &migrate.HttpFileSystemMigrationSource{
	//	FileSystem: pkger.Dir("/migrations"),
	//}
	migrations := &migrate.PackrMigrationSource{
		Box: packr.NewBox("./migrations"),
	}

	db := lib.OpenDB()

	n, err := migrate.Exec(db, "sqlite3", migrations, dir)
	if err != nil {
		panic(err)
	}
	fmt.Printf("Applied %d migrations!\n", n)
}
