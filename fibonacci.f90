module math_fibonacci
    implicit none
    
contains

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
end module math_fibonacci
