# test_lib.py
from iterative_lib import factorial_iterative, gcd_iterative, diophantine_iterative
from recursive_lib import factorial_recursive, gcd_recursive, diophantine_recursive


def test_factorial(input, answer):
    result_iterative = factorial_iterative(input)
    result_recursive = factorial_recursive(input)
    print(f"\t\titerative : {input}! = {result_iterative} : {'TRUE' if result_iterative == answer else 'FALSE'}")
    print(f"\t\trecursive : {input}! = {result_recursive} : {'TRUE' if result_recursive == answer else 'FALSE'}")


def test_gcd(input_x, input_y, answer):
    result_iterative = gcd_iterative(input_x, input_y)
    result_recursive = gcd_recursive(input_x, input_y)
    print(f"\t\titerative : gcd({input_x}, {input_y}) = {result_iterative} : {'TRUE' if result_iterative == answer else 'FALSE'}")
    print(f"\t\trecursive : gcd({input_x}, {input_y}) = {result_recursive} : {'TRUE' if result_recursive == answer else 'FALSE'}")


def test_diophantine(input_a, input_b, input_c):
    sol_iterative = diophantine_iterative(input_a, input_b, input_c)
    sol_recursive = diophantine_recursive(input_a, input_b, input_c)
    
    iterative_condition = (sol_iterative.err is True and input_c % gcd_iterative(input_a, input_b) != 0) or ((input_a * sol_iterative.x) + (input_b * sol_iterative.y) == input_c)
    recursive_condition = (sol_recursive.err is True and input_c % gcd_recursive(input_a, input_b) != 0) or ((input_a * sol_recursive.x) + (input_b * sol_recursive.y) == input_c)
    
    print(f"\t\titerative : diophantine({input_a}x + {input_b}y = {input_c}) = {sol_iterative.x}, {sol_iterative.y} : err = {'true' if sol_iterative.err else 'false'} : {'TRUE' if iterative_condition else 'FALSE'}")
    print(f"\t\trecursive : diophantine({input_a}x + {input_b}y = {input_c}) = {sol_recursive.x}, {sol_recursive.y} : err = {'true' if sol_recursive.err else 'false'} : {'TRUE' if recursive_condition else 'FALSE'}")


print("Predefined tests:\n")

print("\tFactorial:")
test_factorial(0, 1)
test_factorial(1, 1)
test_factorial(4, 24)
test_factorial(5, 120)
test_factorial(14, 87178291200)

print("\tGCD:")
test_gcd(1, 20, 1)
test_gcd(5, 25, 5)
test_gcd(7, 0, 7)
test_gcd(123456, 789, 3)
test_gcd(203167, 296703, 37)

print("\tDiophantine:")
test_diophantine(3, 5, 6)
test_diophantine(8, 6, 11)
test_diophantine(68, 143, 45)
test_diophantine(21, 209, 13)
test_diophantine(31, 747, 90)
