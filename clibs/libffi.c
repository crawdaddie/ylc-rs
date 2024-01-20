#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>

int printd(int i) {
  printf("%d\n", i);
  return 1;
}

// int printstr(char s, char s2, char s3, char s4, char s5) {
//   printf("%c", s);
//   printf("%c", s2);
//   printf("%c", s3);
//   printf("%c", s4);
//   printf("%c\n", s5);
//   return 1;
//
// }
//// Function to print a string of characters
typedef struct {
  char *value;
  int64_t size;
} str;
int printstr(str s) {
  printf("size: %lld\n", s.size);
  printf("size: %s\n", s.value);
  return 1;
}

int printc(char *s) {
  printf("%c\n", *s);
  return 1;
}
