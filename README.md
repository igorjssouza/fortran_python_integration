# Fortran python integration


## This code defines a module factorial_mod, which contains a subroutine factorial. The factorial subroutine takes an integer n as input and computes its factorial, storing the result in the output variable result.

## To compile this Fortran code into a library, you can use a Fortran compiler such as gfortran. Here's how you can compile it into a library named libfactorial.a:

gfortran -c factorial_lib.f90
ar rcs libfactorial.a factorial_lib.o

## After compilation, you'll have a library file named libfactorial.a, which you can link with other Fortran programs to use the factorial subroutine.

## To compile the main program and link it with the library, you can use the following command:

gfortran -o main main.f90 -L. -lfactorial


"-o main": This option specifies the name of the output file (executable) produced by the compilation process. In this case, the output file will be named main.

"main.f90": This is the source file containing the main program written in Fortran. It's the file you want to compile and link into an executable.

"-L.": This option tells the compiler to add the current directory (.) to the list of directories to search for libraries. In this case, it's specifying that the library file libfactorial.a is located in the current directory.

"-lfactorial": This option specifies the name of the library to link with the main program. The -l flag is used to indicate that a library should be linked, followed by the name of the library without the lib prefix and the .a extension. So in this case, it links with the library named factorial.
