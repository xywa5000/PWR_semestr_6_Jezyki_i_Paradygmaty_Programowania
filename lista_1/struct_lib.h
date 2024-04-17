#ifndef STRUCT_LIB_H
#define STRUCT_LIB_H
#include <stdbool.h>

// Struktura do przechowywania rozwiązania równania diofantycznego
typedef struct {
    int x;
    int y;
    bool err;
} Solution;

#endif