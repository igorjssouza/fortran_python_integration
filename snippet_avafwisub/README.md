# Fortran python integration


### Step 1 - Generate the factorial.o and fibonacci.o

gfortran -c avafwisub.f90

### It will produce two output file

avafwisub.o (object file)

## Step 2 - Generate the fibonacci_factorial_lib.a

ar rcs avafwisub.a avafwisub.o

### Step 3 - Create a signature file. First, you need a signature file from the Fortran sources. This step assumes you at least know the interface (subroutine and function definitions) of the Fortran code. If you don't have this information, you'll need to obtain it from the original source code or documentation. Generate the signature file:

f2py3 -m avafwisub_static_lib -h avafwisub_static_lib.pyf avafwisub_interface.f90

### Step 4 - Compile with f2py3 Using Static Library

f2py3 -c avafwisub_static_lib.pyf -L. avafwisub.a

### Run fortran main

gfortran -g -c avafwisub.f90

ar rcs avafwisub.a avafwisub.o

gfortran -g -o test_avafwisub test_avafwisub.f90 avafwisub.a

gdb ./test_avafwisub
