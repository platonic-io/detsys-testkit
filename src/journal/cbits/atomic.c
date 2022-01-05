#include <stdatomic.h>

_Bool
c_atomic_compare_exchange_strong(_Atomic(int) *object, int expected, int desired) {
  return atomic_compare_exchange_strong(object, &expected, desired);
}

// #include <stdio.h> // XXX: remove
// int
// main(void) {
// 	atomic_int acnt = 3;
//   int res;
// 	printf("acnt = %u\n", acnt);
// 	res = c_atomic_compare_exchange_strong(&acnt, 3, 4);
// 	printf("cas(3, 4) = %d\n", res);
// 	res = c_atomic_compare_exchange_strong(&acnt, 2, 5);
// 	printf("cas(2, 5) = %d\n", res);
// 	res = c_atomic_compare_exchange_strong(&acnt, 4, 5);
// 	printf("cas(4, 5) = %d\n", res);
// 	printf("acnt = %u\n", acnt);
// }
