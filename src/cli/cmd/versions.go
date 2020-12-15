package cmd

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"

	"github.com/spf13/cobra"
)

func printVersion(c string) {
	cmd := exec.Command(c, "--version")
	out, err := cmd.CombinedOutput()

	if err != nil {
		fmt.Printf("%s\n%s\n", out, err)
		os.Exit(1)
	}
	if c == "detsys" {
		out = bytes.TrimPrefix(out, []byte("detsys version "))
	}
	fmt.Printf("%16s version: %s", c, out)
}

var versionsCmd = &cobra.Command{
	Use:   "versions",
	Short: "Prints the version of all components",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(_ *cobra.Command, args []string) {
		cmds := []string{
			"detsys",
			"detsys-debug",
			"detsys-db",
			"detsys-checker",
			"detsys-scheduler",
		}
		for _, c := range cmds {
			printVersion(c)
		}
	},
}
