# Fortran python integration


### Step 1 - Generate the factorial.o and fibonacci.o

gfortran -c factorial.f90 fibonacci.f90

### It will produce two output files 

factorial.o and fibonacci.o (object files)

## Step 2 - Generate the fibonacci_factorial_lib.a

ar rcs fibonacci_factorial_lib.a factorial.o fibonacci.o

### Step 3 - Create a signature file. First, you need a signature file from the Fortran sources. This step assumes you at least know the interface (subroutine and function definitions) of the Fortran code. If you don't have this information, you'll need to obtain it from the original source code or documentation. Generate the signature file:

f2py3 -m avafwisub_lib -h avafwisub_lib.pyf avafwisub_interface.f90

### Step 4 - Compile with f2py3 Using Object Files and Static Library

f2py3 -c math_lib.pyf factorial.o fibonacci.o

### Or

f2py3 -c avafwisub_lib.pyf -L. avafwisub.a (our case)

### Run fortran main

gfortran -g -c avafwisub.f90

ar rcs avafwisub.a avafwisub.o

gfortran -g -o test_avafwisub test_avafwisub.f90 avafwisub.a

gdb ./test_avafwisub
