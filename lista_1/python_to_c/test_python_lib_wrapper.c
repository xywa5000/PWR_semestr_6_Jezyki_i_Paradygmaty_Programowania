#include <stdio.h>
#include <stdlib.h>
#include "python_lib_wrapper.h"


void test_factorial(int input, long long answer) {
    long long result_iterative = factorial_iterative_python(input);
    long long result_recursive = factorial_recursive_python(input);

    printf("\t\titerative : %d! = %lld : %s\n", input, result_iterative, (result_iterative == answer) ? "TRUE" : "FALSE");
    printf("\t\trecursive : %d! = %lld : %s\n", input, result_recursive, (result_recursive == answer) ? "TRUE" : "FALSE");
}


void test_gcd(unsigned int input_x, unsigned int input_y, unsigned int answer) {
    unsigned int result_iterative = gcd_iterative_python(input_x, input_y);
    unsigned int result_recursive = gcd_recursive_python(input_x, input_y);

    printf("\t\titerative : gcd(%u, %u) = %u : %s\n", input_x, input_y, result_iterative, (result_iterative == answer) ? "TRUE" : "FALSE");
    printf("\t\trecursive : gcd(%u, %u) = %u : %s\n", input_x, input_y, result_recursive, (result_recursive == answer) ? "TRUE" : "FALSE");
}


void test_diophantine(int input_a, int input_b, int input_c) {
    Solution sol_iterative = diophantine_iterative_python(input_a, input_b, input_c);
    Solution sol_recursive = diophantine_recursive_python(input_a, input_b, input_c);

    bool iterative_condition = (sol_iterative.err == true && input_c % gcd_iterative_python(input_a, input_b) != 0) || ((input_a * sol_iterative.x) + (input_b * sol_iterative.y) == input_c);
    bool recursive_condition = (sol_recursive.err == true && input_c % gcd_recursive_python(input_a, input_b) != 0) || ((input_a * sol_recursive.x) + (input_b * sol_recursive.y) == input_c);

    printf("\t\titerative : diophantine(%dx + %dy = %d) = %d, %d : err = %s : %s\n", input_a, input_b, input_c, sol_iterative.x, sol_iterative.y, sol_iterative.err ? "true" : "false", iterative_condition ? "TRUE" : "FALSE");
    printf("\t\trecursive : diophantine(%dx + %dy = %d) = %d, %d : err = %s : %s\n", input_a, input_b, input_c, sol_recursive.x, sol_recursive.y, sol_recursive.err ? "true" : "false", recursive_condition ? "TRUE" : "FALSE");
}


int main() {

    printf("Predefined tests:\n\n");

    printf("\tFactorial:\n");
    test_factorial(0, 1);
    test_factorial(1, 1);
    test_factorial(4, 24);
    test_factorial(5, 120);
    test_factorial(14, 87178291200);

    printf("\tGCD:\n");
    test_gcd(1, 20, 1);
    test_gcd(5, 25, 5);
    test_gcd(7, 0, 7);
    test_gcd(123456,789, 3);
    test_gcd(203167,296703, 37);

    printf("\tDiophantine:\n");
    test_diophantine(3, 5 , 6);
    test_diophantine(8, 6, 11);
    test_diophantine(68, 143, 45);
    test_diophantine(21, 209, 13);
    test_diophantine(31, 747, 90);

    return 0;
}
