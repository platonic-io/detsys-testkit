//go:build linux
// +build linux

package main

import (
	"syscall"
)

func fallocate(fd int, mode uint32, offset int64, length int64) error {
	return syscall.Fallocate(fd, mode, offset, length)
}
