#include <stdint.h>
#include <stdio.h>

typedef struct {
  double *nums;
  uint32_t size;
} double_arr;

uint32_t array_size(void *arr) {
  double_arr *str = (double_arr *)arr;

  return str->size;
}
