# Running Instructions

Details and requirements are provided in the file **labor1.pdf**.

## Requirements

- Unix based OS (tested on Ubuntu 22.04)
- gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
- Python 3.10.12
- GNATMAKE 10.5.0

## Libraries

### C library

#### Recursive:
```
    gcc -fdiagnostics-color=always -g ./library_c/test_recursive_lib.c ./library_c/recursive_lib.c ./library_c/struct_lib.h -o test_recursive_lib
    ./test_recursive_lib
    rm test_recursive_lib
```

#### Iterative:
```
    gcc -fdiagnostics-color=always -g ./library_c/test_iterative_lib.c ./library_c/iterative_lib.c ./library_c/struct_lib.h -o test_iterative_lib
    ./test_iterative_lib
    rm test_iterative_lib
```

#### All:
```
    gcc -fdiagnostics-color=always -g ./library_c/test_lib.c ./library_c/iterative_lib.c ./library_c/recursive_lib.c ./library_c/struct_lib.h -o test_lib
    ./test_lib
    rm test_lib
```

### Ada library

#### Recursive:
```
    gnatmake ./library_ada/test_recursive_lib.adb
    ./test_recursive_lib
    rm test_recursive_lib *.o *.ali
```

#### Iterative:
```
    gnatmake ./library_ada/test_iterative_lib.adb
    ./test_iterative_lib
    rm test_iterative_lib *.o *.ali
```

#### All:
```
    gnatmake ./library_ada/test_lib.adb
    ./test_lib
    rm test_lib *.o *.ali
```

### Python library

#### Recursive:
```
    python3 ./library_python/test_recursive_lib.py
    rm -r ./library_python/__pycache__
```

#### Iterative:
```
    python3 ./library_python/test_iterative_lib.py
    rm -r ./library_python/__pycache__
```

#### All:
```
    python3 ./library_python/test_lib.py
    rm -r ./library_python/__pycache__
```

## Wrappers

### Ada to C

```
    gnatmake -gnato ./library_ada/iterative_lib.adb ./library_ada/recursive_lib.adb
    gcc -c ./wrapper_ada_to_c/test_ada_lib_wrapper.c ./wrapper_ada_to_c/ada_lib_wrapper.c
    gcc -o test_ada_lib_wrapper test_ada_lib_wrapper.o ada_lib_wrapper.o iterative_lib.o recursive_lib.o struct_lib.o -lgnat
    ./test_ada_lib_wrapper
    rm test_ada_lib_wrapper *.o *.ali
```

### C to Ada

```
    gcc -c ./library_c/iterative_lib.c
	gcc -c ./library_c/recursive_lib.c
	gnatmake ./wrapper_c_to_ada/test_c_lib_wrapper.adb
	x86_64-linux-gnu-gnatlink-10 test_c_lib_wrapper.ali iterative_lib.o recursive_lib.o
    ./test_c_lib_wrapper
    rm test_c_lib_wrapper *.o *.ali
```

### Python to C

```
    gcc -o test_python_lib_wrapper ./wrapper_python_to_c/test_python_lib_wrapper.c ./wrapper_python_to_c/python_lib_wrapper.c -I/usr/include/python3.10 -lpython3.10
    ./test_python_lib_wrapper
    rm test_python_lib_wrapper
```

### C to Python

```
    gcc -shared -o iterative_lib.so ./library_c/iterative_lib.c -fPIC
    gcc -shared -o recursive_lib.so ./library_c/recursive_lib.c -fPIC
    python3 ./wrapper_c_to_python/test_c_lib_wrapper.py
    rm *.so
    rm -r ./wrapper_c_to_python/__pycache__
```