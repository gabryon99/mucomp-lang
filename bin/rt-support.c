#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INPUT_SIZE 256

int getint(){

    char buffer[INPUT_SIZE];
    if (fgets(buffer, INPUT_SIZE, stdin) == NULL) return 0;
  
    return (int) strtol(buffer, NULL, 10);
}

void abort(int n) {
  exit(n);
}

void print(int n){
  fprintf(stdout, "%d\n", n);
}

void print_err(int n) {
  fprintf(stderr, "%d\n", n);
}