# Fortran python integration


### This code defines a module factorial_mod, which contains a subroutine factorial. The factorial subroutine takes an integer n as input and computes its factorial, storing the result in the output variable result.

### To compile this Fortran code into a library, you can use a 2py3 Python numpy library. Pay attention to number 3 in the end of f2py3, it represents the curret python version.

f2py3 -c -m fibonacci_factorial_lib fibonacci_factorial_lib.f90

### After compilation, you'll have a library file named fibonacci_factorial_lib.so to use in python script.

### to usu inside yout python script:
import fibonacci_factorial_lib
