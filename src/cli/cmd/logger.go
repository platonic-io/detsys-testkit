package cmd

import (
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"strconv"

	"github.com/spf13/cobra"
)

var loggerCmd = &cobra.Command{
	Use:   "logger [command]",
	Short: "Start or stop the logger",
	Long:  ``,
	Args:  cobra.ExactArgs(1),
}

var pidLogger string = pidFile("logger")

var loggerUpCmd = &cobra.Command{
	Use:   "up",
	Short: "Start the logger",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(_ *cobra.Command, args []string) {
		pid, err := readPid("logger")
		if err == nil {
			fmt.Printf("Already running on pid: %d (%s)\n", pid, pidLogger)
			os.Exit(1)
		}

		cmd := exec.Command("detsys-logger")

		err = cmd.Start()

		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}

		pidBs := []byte(strconv.Itoa(cmd.Process.Pid))

		ioutil.WriteFile(pidLogger, pidBs, 0600)
		fmt.Printf("%s\n", pidBs)
	},
}

var loggerDownCmd = &cobra.Command{
	Use:   "down",
	Short: "Stop the logger",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(_ *cobra.Command, args []string) {

		pid, err := readPid("logger")
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

		err = os.Remove(pidLogger)
		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}
	},
}
