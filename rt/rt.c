#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct {
  void* eax; // 0   scratch  (caller-save)
  void* ebx; // 4   preserve (callee-save)
  void* ecx; // 8   scratch
  void* edx; // 12  scratch
  void* esi; // 16  preserve
  void* edi; // 20  preserve
  void* ebp; // 24  preserve
  void* esp; // 28  preserve
} context;

typedef unsigned int ptr;
extern ptr scheme_entry(context* ctx, char* stack, char* heap);

extern char* allocate_protected_space(int size);
extern void deallocate_protected_space(char* p, int size);

#define ARRAY_LENGTH(x) (sizeof((x))/sizeof((x)[0]))

#define CONST_FALSE   0b00101111
#define CONST_TRUE    0b01101111
#define CONST_NULL    0b00111111

#define CHAR_TAG      0b00001111
#define CHAR_TAG_MASK 0b11111111
#define CHAR_SHIFT             8

#define PAIR_TAG           0b001
#define PAIR_TAG_MASK      0b111

#define FIXNUM_SHIFT           2

struct {
  char c;
  char* s;
} escape_sequences[] = {
  {'\t', "\\t"},
  {'\r', "\\r"},
  {'\n', "\\n"},
  {'\'', "\\'"},
  {'\\', "\\\\"},
};

static void print_ptr(ptr x) {
  if (x == CONST_FALSE) {
    printf("false");

  } else if (x == CONST_TRUE) {
    printf("true");

  } else if (x == CONST_NULL) {
    printf("()");

  } else if ((x & CHAR_TAG_MASK) == CHAR_TAG) {
    char c = (char)(x >> CHAR_SHIFT);
    bool match = false;
    for (int i=0; i < ARRAY_LENGTH(escape_sequences); ++i) {
      if (escape_sequences[i].c == c) {
        printf("'%s'", escape_sequences[i].s);
        match = true;
        break;
      }
    }
    if (!match) printf("'%c'", c);

  } else if ((x & PAIR_TAG_MASK) == PAIR_TAG) {
    ptr* pair = (ptr*)(x & ~PAIR_TAG_MASK);
    printf("(");
    print_ptr(pair[0]);
    printf(" . ");
    print_ptr(pair[1]);
    printf(")");

  } else {
    int v = ((int) x) >> FIXNUM_SHIFT;
    printf("%d", v);
  }
}

int main(int argc, char** argv) {
  int stack_size = 4 * 4096;
  int heap_size = 16 * 4096;
  char* stack_top = allocate_protected_space(stack_size);
  char* stack_base = stack_top + stack_size;
  char* heap = allocate_protected_space(heap_size);
  context ctx;
  print_ptr(scheme_entry(&ctx, stack_base, heap));
  deallocate_protected_space(stack_top, stack_size);
  deallocate_protected_space(heap, heap_size);
  return 0;
}
