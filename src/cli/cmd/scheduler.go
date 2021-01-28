package cmd

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"strconv"

	"github.com/spf13/cobra"
	"github.com/symbiont-io/detsys-testkit/src/lib"
)

var schedulerCmd = &cobra.Command{
	Use:   "scheduler [command]",
	Short: "Start or stop the scheduler",
	Long:  ``,
	Args:  cobra.ExactArgs(1),
}

var pidScheduler string = pidFile("scheduler")

var schedulerUpCmd = &cobra.Command{
	Use:   "up",
	Short: "Start the scheduler",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(_ *cobra.Command, args []string) {
		pid, err := readPid("scheduler")
		if err == nil {
			fmt.Printf("Already running on pid: %d (%s)\n", pid, pidScheduler)
			os.Exit(1)
		}

		cmd := exec.Command("detsys-scheduler")

		err = cmd.Start()

		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}

		pidBs := []byte(strconv.Itoa(cmd.Process.Pid))

		ioutil.WriteFile(pidScheduler, pidBs, 0600)
		fmt.Printf("%s\n", pidBs)
	},
}

var schedulerDownCmd = &cobra.Command{
	Use:   "down",
	Short: "Stop the scheduler",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(_ *cobra.Command, args []string) {

		pid, err := readPid("scheduler")
		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}

		process, err := os.FindProcess(pid)

		// TODO(stevan): catch `os: process already finished` and remove
		// pid file and exit 0. (And do the same for logger, or better
		// yet factor out this code.)
		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}

		err = process.Kill()
		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}

		err = os.Remove(pidScheduler)
		if err != nil {
			fmt.Printf("%s\n", err)
			os.Exit(1)
		}
	},
}

var schedulerStatusCmd = &cobra.Command{
	Use:   "status",
	Short: "Show the status of the scheduler",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(cmd *cobra.Command, args []string) {
		json, err := json.Marshal(lib.Status())
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}
		fmt.Println(string(json))
	},
}

var schedulerResetCmd = &cobra.Command{
	Use:   "reset",
	Short: "Reset the state of the scheduler",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(cmd *cobra.Command, args []string) {
		lib.Reset()
	},
}

var schedulerLoadCmd = &cobra.Command{
	Use:   "load",
	Short: "Load a test case into the scheduler",
	Long:  ``,
	Args:  cobra.ExactArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		testId, err := lib.ParseTestId(args[0])
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}
		queueSize := lib.LoadTest(testId)
		fmt.Printf("Test case loaded, current queue size: %d\n", queueSize)
	},
}

var schedulerRegisterCmd = &cobra.Command{
	Use:   "register",
	Short: "Register the executor in the scheduler",
	Long:  ``,
	Args:  cobra.ExactArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		testId, err := lib.ParseTestId(args[0])
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}
		lib.Register(testId)
	},
}

var schedulerCreateRunCmd = &cobra.Command{
	Use:   "create-run",
	Short: "Create a new run id",
	Long:  ``,
	Args:  cobra.ExactArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		testId, err := lib.ParseTestId(args[0])
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}

		// TODO(stevan): make this configurable via flags.
		runEvent := lib.CreateRunEvent{
			Seed:          lib.Seed(4),
			Faults:        lib.Faults{},
			TickFrequency: 10000000,
			MinTimeNs:     0,
			MaxTimeNs:     0,
		}
		runId := lib.CreateRun(testId, runEvent)
		fmt.Printf("Created run id: %v\n", runId)
	},
}

var schedulerStepCmd = &cobra.Command{
	Use:   "step",
	Short: "Execute a single step of the currently loaded test case",
	Long:  ``,
	Args:  cobra.NoArgs,
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Println(string(lib.Step()))
	},
}
