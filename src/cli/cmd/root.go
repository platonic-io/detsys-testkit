package cmd

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
)

var rootCmd = &cobra.Command{
	Use:   "detsys",
	Short: "Command-line interface for deterministic system tests library",
	Long:  ``,
}

func init() {
	rootCmd.AddCommand(statusCmd)
	rootCmd.AddCommand(dbCmd)
	dbCmd.AddCommand(dbInitCmd)
	dbCmd.AddCommand(dbDropTablesCmd)
	dbCmd.AddCommand(dbResetCmd)
	dbCmd.AddCommand(dbShellCmd)
	rootCmd.AddCommand(debugCmd)
	rootCmd.AddCommand(schedulerCmd)
}

func Execute() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
