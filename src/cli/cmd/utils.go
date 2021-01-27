package cmd

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strconv"
)

func pidFile(name string) string {
	return filepath.Join(os.TempDir(), fmt.Sprintf("detsys-%s.pid", name))
}

func readPid(name string) (int, error) {
	bs, err := ioutil.ReadFile(pidFile(name))
	if err != nil {
		return -1, err
	}

	pid, err := strconv.Atoi(string(bs))
	if err != nil {
		return -1, err
	}
	return pid, nil
}
