package cmd

import (
	"encoding/json"
	"fmt"
	"log"

	"github.com/spf13/cobra"
	"github.com/symbiont-io/detsys/lib"
)

var statusCmd = &cobra.Command{
	Use:   "status",
	Short: "Show the status of the test infrastructure",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(cmd *cobra.Command, args []string) {
		json, err := json.Marshal(lib.Status())
		if err != nil {
			log.Fatalln(err)
		}
		fmt.Println(string(json))
	},
}
