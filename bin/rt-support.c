#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFF_SIZE 8
#define INPUT_SIZE 256

int __prelude_getint() {

    char buffer[INPUT_SIZE];
    if (fgets(buffer, INPUT_SIZE, stdin) == NULL) return 0;
  
    return (int) strtol(buffer, NULL, 10);
}

char __prelude_getchar() {

  char buffer[BUFF_SIZE];
  if (fgets(buffer, BUFF_SIZE, stdin) == NULL) return '\0';

  return buffer[0];
}

void __prelude_abort(int n) {
  exit(n);
}

void __prelude_print(int n){
  fprintf(stdout, "%d\n", n);
}

void __prelude_putc(char c) {
  fprintf(stdout, "%c", c);
}

void __prelude_putendl() {
  fprintf(stdout, "\n");
}

void __prelude_print_err(int n) {
  fprintf(stderr, "%d\n", n);
}