package cmd

import (
	"fmt"
	"os"
	"os/exec"

	"github.com/spf13/cobra"
)

var dbCmd = &cobra.Command{
	Use:   "db [command]",
	Short: "Interact with the database",
	Long:  ``,
}

var dbInitCmd = &cobra.Command{
	Use:   "init",
	Short: "Initialise the database",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(_ *cobra.Command, args []string) {
		cmd := exec.Command("../db/db.sh", "init")

		out, err := cmd.CombinedOutput()

		if err != nil {
			fmt.Printf("%s\n", out)
			os.Exit(1)
		}
	},
}

var dbDropTablesCmd = &cobra.Command{
	Use:   "drop-tables",
	Short: "Drop all tables in the database",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(_ *cobra.Command, args []string) {
		cmd := exec.Command("../db/db.sh", "drop_tables")

		out, err := cmd.CombinedOutput()

		if err != nil {
			fmt.Printf("%s\n", out)
			os.Exit(1)
		}
	},
}

var dbResetCmd = &cobra.Command{
	Use:   "reset",
	Short: "Drop all tables and then initialise the database",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(_ *cobra.Command, args []string) {
		cmd := exec.Command("../db/db.sh", "reset")

		out, err := cmd.CombinedOutput()

		if err != nil {
			fmt.Printf("%s\n", out)
			os.Exit(1)
		}
	},
}

var dbShellCmd = &cobra.Command{
	Use:   "shell",
	Short: "Start interactive shell session with the database",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(_ *cobra.Command, args []string) {
		cmd := exec.Command("sqlite3", "-interactive", "../db/detsys.sqlite3")

		// These three lines are important, or else `cmd.Run()` will
		// just immediately exit.
		cmd.Stdin = os.Stdin
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr

		err := cmd.Run()

		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}
	},
}
