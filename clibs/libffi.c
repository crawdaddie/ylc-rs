#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

int printd(int i) {
  printf("%d\n", i);
  return 1;
}

// int printstr(char s1, char rest) {
// if (rest == '\0') {
//   putchar(s1);
//   putchar('\n');
//   return 1;
// }
// putchar(s1);
// return printstr(rest, *(&rest - 1));

// char *start = (char *)&s1;
// va_list arg_list;
// va_start(arg_list, s1);
// while (*start != '\0') {
//   putchar(*start);
//   start--;
// }
// printf("\n");
// va_end();

// printf("%c; %p -- %d --\n", s1, (char *)&s1, (char *)&s1 - start);
// printf("%c; %p -- %d --\n", s2, (char *)&s2, (char *)&s2 - start);
// printf("%c; %p -- %d --\n", s3, (char *)&s3, (char *)&s3 - start);
// printf("%c; %p -- %d --\n", s4, (char *)&s4, (char *)&s4 - start);
// printf("%c; %p -- %d --\n", s5, (char *)&s5, (char *)&s5 - start);
// printf("%c; %p -- %d --\n", s6, (char *)&s6, (char *)&s6 - start);
// printf("\n");
// }
int printstr(char *str) {
  // va_list arg_list;
  // va_start(arg_list, num_args);
  //
  // char str = ((char)va_arg(arg_list, int));
  //
  // for (int i = 0; i < num_args; ++i) {
  //   // const char str = va_arg(arg_list, int);
  //   printf("%c; %p\n", str, (void *)&str);
  //   str = *(&str - 1);
  //
  //   printf("\n");
  // }
  //
  // va_end(arg_list);
  printf("%s\n", str);
  return 1;
}

int printfmt(char *fmt, int n, ...) {
  // va_list arg_list;
  // va_start(arg_list, n);
  //
  // // char str = ((char)va_arg(arg_list, int));
  //
  // for (int i = 0; i < n; ++i) {
  //   const char str = va_arg(arg_list, int);
  //   printf("%c; %p\n", str, (void *)&str);
  //   str = *(&str - 1);
  //
  //   printf("\n");
  // }
  //
  // va_end(arg_list);
  printf("fmt: %s %d\n", fmt, n);
  return 1;
}

// int printfmt(char *fmt, ) {
//   printf("%c\n", s);
//   return 1;
// }

int printc(char s) {
  printf("%c\n", s);
  return 1;
}
