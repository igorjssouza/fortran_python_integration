module math_factorial
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
end module math_factorial
