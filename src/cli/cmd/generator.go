package cmd

import (
	"fmt"
	"os"
	"os/exec"

	"github.com/spf13/cobra"
)

var generateCmd = &cobra.Command{
	Use:   "generate",
	Short: "Generate a test case",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(_ *cobra.Command, args []string) {
		cmd := exec.Command("detsys-generator")

		err := cmd.Run()

		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}
	},
}
