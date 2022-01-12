#include <fcntl.h>
#include <unistd.h>

// based on https://github.com/coilhq/tigerbeetle/blob/7e12fccc4859f035cd26af29b8e9f9749a0899a3/src/storage.zig#L430
int mac_fallocate(int fd, off_t offset, off_t length) {
  fstore_t store = {
    .fst_flags = F_ALLOCATECONTIG | F_ALLOCATEALL,
    .fst_posmode = F_PEOFPOSMODE,
    .fst_offset = 0,
    .fst_length = offset + length,
    .fst_bytesalloc = 0,
  };

  int res = fcntl(fd, F_PREALLOCATE, &store);
  if (res != 0) {
    store.fst_flags = F_ALLOCATEALL;
    res = fcntl(fd, F_PREALLOCATE, &store);
  }

  if (res != 0) {
    return res;
  }

  return ftruncate(fd, length);
}
