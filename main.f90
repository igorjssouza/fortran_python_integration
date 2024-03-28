program main
    use factorial_mod
    implicit none
    integer :: n = 5
    integer :: result

    ! Compute the factorial of n
    call factorial (n, result)

    ! Print the result
    print *, "Factorial of", n, "is", result
end program main
