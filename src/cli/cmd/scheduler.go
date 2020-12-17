package cmd

import (
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"

	"github.com/spf13/cobra"
)

const pidFile = "detsys-scheduler.pid"

var schedulerCmd = &cobra.Command{
	Use:   "scheduler [command]",
	Short: "Start or stop the scheduler",
	Long:  ``,
	Args:  cobra.ExactArgs(1),
}

var schedulerUpCmd = &cobra.Command{
	Use:   "up",
	Short: "Start the scheduler",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(_ *cobra.Command, args []string) {
		cmd := exec.Command("detsys-scheduler")

		err := cmd.Start()

		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}

		pid := []byte(strconv.Itoa(cmd.Process.Pid))

		ioutil.WriteFile(filepath.Join(os.TempDir(), pidFile), pid, 0600)
		fmt.Printf("%s\n", pid)
	},
}

var schedulerDownCmd = &cobra.Command{
	Use:   "down",
	Short: "Stop the scheduler",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(_ *cobra.Command, args []string) {
		bs, err := ioutil.ReadFile(filepath.Join(os.TempDir(), pidFile))
		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}

		pid, err := strconv.Atoi(string(bs))
		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}

		process, err := os.FindProcess(pid)
		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}

		err = process.Kill()
		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}
	},
}
