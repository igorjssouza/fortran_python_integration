module math_functions
    implicit none
    
contains
    ! Subroutine to compute the catorial of a given integer
    subroutine factorial(n, result)
        integer, intent(in) :: n
        integer, intent(out) :: result
        integer :: i

        result = 1
        do i= 2, n
            result = result *i
        end do
    end subroutine factorial

    ! Function to compute the nth Fibonacci number
    integer function fibonacci(n)
        integer, intent(in) :: n
        integer :: i, a, b, temp

        a = 0
        b = 1

        if (n==0) then
            fibonacci = a
        else if (n == 1) then
            fibonacci = b
        else 
            do i = 2, n
                temp = b
                b = a + b
                a = temp
            end do
            fibonacci = b
        end if
    end function fibonacci
end module math_functions
