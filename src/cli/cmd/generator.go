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
	Args:  cobra.ExactArgs(1),
	Run: func(_ *cobra.Command, args []string) {
		cmd := exec.Command("detsys-generator", args[0])

		out, err := cmd.CombinedOutput()

		if err != nil {
			fmt.Printf("%s\n%s\n", out, err)
			os.Exit(1)
		}

		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}
	},
}
