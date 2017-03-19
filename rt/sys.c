#include <unistd.h>
#include <sys/mman.h>
#include <stdlib.h>

char* allocate_protected_space(int size) {
  int page = getpagesize();
  int aligned_size = ((size + page - 1) / page) * page;
  char* p = mmap(0, aligned_size + 2 * page, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, 0, 0);
  if (p == MAP_FAILED) abort();

  int status = mprotect(p, page, PROT_NONE);
  if (status != 0) abort();

  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if (status != 0) abort();

  return (p + page);
}

void deallocate_protected_space(char* p, int size) {
  int page = getpagesize();
  int aligned_size = ((size + page - 1) / page) * page;
  int status = munmap(p - page, aligned_size + 2 * page);
  if (status != 0) abort();
}
