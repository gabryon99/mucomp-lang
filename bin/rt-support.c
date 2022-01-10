#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <stddef.h>
#include <stdbool.h>

#define MUCOMP_MAX_INT 0x7FFFFFFF

#define BUFF_SIZE 8
#define INPUT_SIZE 256

int32_t __prelude_time() {
  return (int32_t) time(NULL);
}

void __prelude_set_rand_seed(uint32_t seed) {
  srand(seed);
}

int32_t __prelude_ifloat(float num) {
  return (int32_t) num;
}

float __prelude_fint(int32_t num) {
  return (float) num;
}

int32_t __prelude_rand() {
  #ifdef __OSX__
    return (int32_t) arc4random();
  #else
    return (int32_t) rand();
  #endif
}

int32_t __prelude_getint() {

    char buffer[INPUT_SIZE];
    if (fgets(buffer, INPUT_SIZE, stdin) == NULL) return 0;
  
    return (int) strtol(buffer, NULL, 10);
}

float __prelude_getfloat() {

    char buffer[INPUT_SIZE];
    if (fgets(buffer, INPUT_SIZE, stdin) == NULL) return 0;
  
    return strtof(buffer, NULL);
}

char __prelude_getchar() {

  char buffer[BUFF_SIZE];
  if (fgets(buffer, BUFF_SIZE, stdin) == NULL) return '\0';

  return buffer[0];
}

void __prelude_abort(int32_t n) {
  exit(n);
}

void __prelude_print(int32_t n){
  fprintf(stdout, "%d\n", n);
}

void __prelude_print_f(float n){
  fprintf(stdout, "%.4f\n", n);
}

void __prelude_putc(char c) {
  fprintf(stdout, "%c", c);
}

void __prelude_putendl() {
  fprintf(stdout, "\n");
}
