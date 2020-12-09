package cmd

import (
	"fmt"
	"os"
	"os/exec"

	"github.com/spf13/cobra"
)

var schedulerCmd = &cobra.Command{
	Use:   "scheduler [up]",
	Short: "Start scheduler component",
	Long:  ``,
	Args:  cobra.ExactArgs(1),
	Run: func(_ *cobra.Command, args []string) {
		if args[0] != "up" {
			fmt.Println("invalid subcommand")
			os.Exit(1)
		}
		cmd := exec.Command("detsys-scheduler")

		err := cmd.Start()

		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}
		fmt.Printf("%d", cmd.Process.Pid)
	},
}
