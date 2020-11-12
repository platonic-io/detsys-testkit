package cmd

import (
	"fmt"
	"os"
	"os/exec"

	"github.com/spf13/cobra"
	"github.com/symbiont-io/detsys/lib"
)

var dbCmd = &cobra.Command{
	Use:   "db [command]",
	Short: "Interact with the database",
	Long:  ``,
}

var dbInitCmd = &cobra.Command{
	Use:   "up",
	Short: "Migrate the database upwards",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(_ *cobra.Command, args []string) {
		cmd := exec.Command("detsys-db", "up")

		out, err := cmd.CombinedOutput()

		if err != nil {
			fmt.Printf("%s\n", out)
			os.Exit(1)
		}
		fmt.Printf("%s", out)
	},
}

var dbDropTablesCmd = &cobra.Command{
	Use:   "down",
	Short: "Migrate the database downwards",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(_ *cobra.Command, args []string) {
		cmd := exec.Command("detsys-db", "down")

		out, err := cmd.CombinedOutput()

		if err != nil {
			fmt.Printf("%s\n", out)
			os.Exit(1)
		}
		fmt.Printf("%s", out)
	},
}

var dbResetCmd = &cobra.Command{
	Use:   "reset",
	Short: "Migrate the database down- and then upwards",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(_ *cobra.Command, args []string) {
		cmd := exec.Command("detsys-db", "down")
		err := cmd.Run()
		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}
		cmd = exec.Command("detsys-db", "up")
		out, err := cmd.CombinedOutput()
		if err != nil {
			fmt.Printf("%s\n", out)
			os.Exit(1)
		}
		fmt.Printf("%s", out)
	},
}

var dbShellCmd = &cobra.Command{
	Use:   "shell",
	Short: "Start interactive shell session with the database",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(_ *cobra.Command, args []string) {
		cmd := exec.Command("sqlite3", "-interactive", lib.DBPath())

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
