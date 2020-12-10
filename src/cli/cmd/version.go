package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
)

var version string

var versionCmd = &cobra.Command{
	Use:   "version",
	Short: "Returns the version of this executable",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(cmd *cobra.Command, args []string) {
		if version == "" {
			version = "unknown"
		}
		fmt.Fprintln(cmd.OutOrStdout(), version)
	},
}
