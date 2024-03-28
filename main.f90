program main
    use math_functions
    implicit none
    integer :: n
    integer :: result

    write(*,*) "Enter the number: "
    read(*,*) n

    ! Compute the factorial of n
    call factorial (n, result)

    ! Print the result
    print *, "Factorial of", n, "is", result

    ! Compute the nth Fibonacci number
    print *, n, "th Fibonacci number is", fibonacci(n)
end program main
