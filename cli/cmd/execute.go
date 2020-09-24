package cmd

import (
	"fmt"
	"log"

	"github.com/spf13/cobra"
	"github.com/symbiont-io/detsys/lib"
)

var executeCmd = &cobra.Command{
	Use:   "execute [test-id]",
	Short: "Execute a test",
	Long:  ``,
	Args:  cobra.ExactArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		testId, err := lib.ParseTestId(args[0])
		if err != nil {
			log.Fatalln(err)
		}
		lib.Execute(testId)
	},
}
