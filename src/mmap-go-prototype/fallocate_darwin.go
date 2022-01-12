//go:build darwin
// +build darwin

package main

import (
	"os"
	"syscall"
	"unsafe"
)

// based on https://github.com/coilhq/tigerbeetle/blob/7e12fccc4859f035cd26af29b8e9f9749a0899a3/src/storage.zig#L430
func fallocate(fd int, _mode uint32, offset int64, length int64) error {
	store := syscall.Fstore_t{
		Flags:      syscall.F_ALLOCATECONTIG | syscall.F_ALLOCATEALL,
		Posmode:    syscall.F_PEOFPOSMODE,
		Offset:     0,
		Length:     offset + length,
		Bytesalloc: 0,
	}
	_, _, err := syscall.Syscall(syscall.SYS_FCNTL, fd, syscall.F_PREALLOCATE, uintptr(unsafe.Pointer(&store)))
	if err != 0 {
		store.Flags = syscall.F_ALLOCATEALL
		_, _, err = syscall.Syscall(syscall.SYS_FCNTL, fd, syscall.F_PREALLOCATE, uintptr(unsafe.Pointer(&store)))
		if err != 0 {
			return err
		}
	}

	return syscall.Ftruncate(fd, store.Length)
}
