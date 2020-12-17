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

var schedulerCmd = &cobra.Command{
	Use:   "scheduler [command]",
	Short: "Start or stop the scheduler",
	Long:  ``,
	Args:  cobra.ExactArgs(1),
}

func pidFile() string {
	return filepath.Join(os.TempDir(), "detsys-scheduler.pid")
}

var schedulerUpCmd = &cobra.Command{
	Use:   "up",
	Short: "Start the scheduler",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(_ *cobra.Command, args []string) {
		pid, err := readPid()
		if err == nil {
			fmt.Printf("Already running on pid: %d (%s)\n", pid, pidFile())
			os.Exit(1)
		}

		cmd := exec.Command("detsys-scheduler")

		err = cmd.Start()

		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}

		pidBs := []byte(strconv.Itoa(cmd.Process.Pid))

		ioutil.WriteFile(pidFile(), pidBs, 0600)
		fmt.Printf("%s\n", pidBs)
	},
}

func readPid() (int, error) {
	bs, err := ioutil.ReadFile(pidFile())
	if err != nil {
		return -1, err
	}

	pid, err := strconv.Atoi(string(bs))
	if err != nil {
		return -1, err
	}
	return pid, nil
}

var schedulerDownCmd = &cobra.Command{
	Use:   "down",
	Short: "Stop the scheduler",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(_ *cobra.Command, args []string) {

		pid, err := readPid()
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

		err = os.Remove(pidFile())
		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}
	},
}
