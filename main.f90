program main
    use factorial_mod
    implicit none
    integer :: n
    integer :: result

    write(*,*) "Enter the number: "
    read(*,*) n

    ! Compute the factorial of n
    call factorial (n, result)

    ! Print the result
    print *, "Factorial of", n, "is", result
end program main
