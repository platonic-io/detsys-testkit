package cmd

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
)

var rootCmd = &cobra.Command{
	Use:   "detsys",
	Short: "Command-line interface for determinstic system tests library",
	Long:  ``,
}

func init() {
	rootCmd.AddCommand(executeCmd)
}

func Execute() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
