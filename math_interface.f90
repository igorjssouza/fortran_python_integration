! -- file: math_interface.f90
module math_factorial
    contains
        subroutine factorial(n, result)
            integer, intent(in) :: n
            integer, intent(out) :: result
        end subroutine factorial
    end module math_factorial
    
module math_fibonacci
    contains
        integer function fibonacci(n)
            integer, intent(in) :: n
        end function fibonacci
    end module math_fibonacci
    