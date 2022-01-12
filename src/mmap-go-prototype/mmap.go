package main

import (
	"encoding/binary"
	"fmt"
	"math"
	"os"
	"sync/atomic"
	"syscall"
	"unsafe"
)

type Mmap struct {
	data []byte
}

func NewMmap(f string) *Mmap {
	file, err := os.OpenFile(f, os.O_RDWR|os.O_CREATE, 0644)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	pageSize := syscall.Getpagesize()

	// XXX: Passing `FALLOC_FL_ZERO_RANGE` as mode would zero the file, but
	// I couldn't find this constant in `syscall`...
	mode := uint32(0)

	if err := syscall.Fallocate(int(file.Fd()), mode, 0, int64(pageSize)); err != nil {
		panic(err)
	}

	data, err := syscall.Mmap(int(file.Fd()), 0, int(pageSize),
		syscall.PROT_READ|syscall.PROT_WRITE, syscall.MAP_SHARED)
	if err != nil {
		panic(err)
	}
	return &Mmap{data: data}
}

func (m *Mmap) Free() {
	if err := syscall.Munmap(m.data); err != nil {
		panic(err)
	}
}

func main() {
	f := "/tmp/test_mmap.txt"
	m := NewMmap(f)
	m.data[0] = 'A'
	m.data[1] = 'B'
	m.data[2] = 'C'
	// `sync/atomic` doesn't have support for working against `byte[]`,
	// hence the use of unsafe pointers.
	uptr := unsafe.Pointer(&m.data[0])
	atomic.AddUint32((*uint32)(uptr), 1)                            // A => B
	atomic.AddUint32((*uint32)(unsafe.Pointer(uintptr(uptr)+2)), 2) // C => E

	// XXX: couldn't get the following block of code to work yet:
	bs := make([]byte, 4)
	binary.BigEndian.PutUint32(bs[:4], math.MaxUint32-1)
	fmt.Printf("math.MaxUint32-1: %d\n", math.MaxUint32-1)
	copy(m.data[3:], bs)
	atomic.AddUint32((*uint32)(unsafe.Pointer(uintptr(uptr)+3)), 1)
	i := binary.BigEndian.Uint32(m.data[3:7])
	fmt.Printf("i: %d\n", i) // prints 255 rather than `MaxUint32`

	m.Free()
	data, err := os.ReadFile(f)
	if err != nil {
		panic(err)
	}
	os.Stdout.Write(data) // Prints "BBE".
}
