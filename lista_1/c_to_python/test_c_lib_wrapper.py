# test_wrapper.py
from c_lib_wrapper import factorial_iterative_c, gcd_iterative_c, diophantine_iterative_c, factorial_recursive_c, gcd_recursive_c, diophantine_recursive_c


def test_factorial(input, answer):
    result_iterative = factorial_iterative_c(input)
    result_recursive = factorial_recursive_c(input)
    print(f"\t\titerative : {input}! = {result_iterative} : {'TRUE' if result_iterative == answer else 'FALSE'}")
    print(f"\t\trecursive : {input}! = {result_recursive} : {'TRUE' if result_recursive == answer else 'FALSE'}")


def test_gcd(input_x, input_y, answer):
    result_iterative = gcd_iterative_c(input_x, input_y)
    result_recursive = gcd_recursive_c(input_x, input_y)
    print(f"\t\titerative : gcd({input_x}, {input_y}) = {result_iterative} : {'TRUE' if result_iterative == answer else 'FALSE'}")
    print(f"\t\trecursive : gcd({input_x}, {input_y}) = {result_recursive} : {'TRUE' if result_recursive == answer else 'FALSE'}")


def test_diophantine(input_a, input_b, input_c, expected_solution):
    solution_iterative = diophantine_iterative_c(input_a, input_b, input_c)
    solution_recursive = diophantine_recursive_c(input_a, input_b, input_c)
    condition_iterative = (solution_iterative.err is True and input_c % gcd_iterative_c(input_a, input_b) != 0) or ((input_a * solution_iterative.x) + (input_b * solution_iterative.y) == input_c)
    condition_recursive = (solution_recursive.err is True and input_c % gcd_iterative_c(input_a, input_b) != 0) or ((input_a * solution_recursive.x) + (input_b * solution_recursive.y) == input_c)
    print(f"\t\titerative : diophantine({input_a}x + {input_b}y = {input_c}) = {solution_iterative.x}, {solution_iterative.y} : err = {'true' if solution_iterative.err else 'false'} : {'TRUE' if condition_iterative else 'FALSE'}")
    print(f"\t\trecursive : diophantine({input_a}x + {input_b}y = {input_c}) = {solution_recursive.x}, {solution_recursive.y} : err = {'true' if solution_recursive.err else 'false'} : {'TRUE' if condition_recursive else 'FALSE'}")


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
test_diophantine(3, 5, 6, (1, 1, False))
test_diophantine(8, 6, 11, (1, -1, False))
test_diophantine(68, 143, 45, (0, 45, False))
test_diophantine(21, 209, 13, (-13, 1, False))
test_diophantine(31, 747, 90, (90, -3, False))
