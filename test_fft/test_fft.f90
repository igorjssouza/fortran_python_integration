program test_fft_box_function
    implicit none
 
    complex, allocatable :: x(:)
    integer :: n, i
    character(len=30) :: filename, box_filename
  
    ! Define the size of the array (must be a power of two)
    n = 128
  
    ! Allocate the array
    allocate(x(n))
  
    ! Initialize the box function (rectangular function)
    do i = 1, n
       if (i <= n/2) then
          x(i) = cmplx(0.0, 0.0)
       else
          x(i) = cmplx(1.0, 0.0)
       end if
    end do

    ! Specify the box function output file name
    box_filename = 'box_function.dat'
  
    ! Write the box function to a file
    open(unit=10, file=box_filename, status='replace')
    do i = 1, n
       write(10,*) real(x(i)), aimag(x(i))
    end do
    close(10)
  
    print *, "Box function saved to ", trim(box_filename)
  
    ! Call the FFT subroutine
    call fft(x, n)
  
    ! Specify the output file name
    filename = 'sync_function.dat'
  
    ! Write the transformed array (FFT result) to a file
    open(unit=10, file=filename, status='replace')
    do i = 1, n
       write(10,*) real(x(i)), aimag(x(i))
    end do
    close(10)
  
    print *, "FFT result saved to ", trim(filename)
 
 contains
 
 ! A subroutine to perform the Fast Fourier Transform (FFT)
 subroutine fft(x, n)
    implicit none
    ! Inputs/Outputs:
    ! x: Complex array to be transformed in place
    ! n: Length of the complex array x, must be a power of two
 
    complex, intent(inout) :: x(:)  ! Input array, which will hold the FFT results
    integer, intent(in) :: n        ! The size of the input array
    integer :: i, j, k, n1, n2      ! Loop counters and control variables
    complex :: t, u                 ! Temporary complex variables for transformations
    real :: pi                      ! Constant pi
    
    pi = 3.14159265358979323846     ! Define the value of pi
 
    ! Bit-reversal permutation implementation
    j = 0
    do i = 0, n - 2
        if (i < j) then
            ! Swap elements in positions i and j
            t = x(j+1)
            x(j+1) = x(i+1)
            x(i+1) = t
        end if
        ! Update j to the next value
        k = n / 2
        do while (k <= j)
            j = j - k
            k = k / 2
        end do
        j = j + k
    end do
 
    ! Main FFT computation
    n1 = 0
    n2 = 1
 
    ! Outer loop over the FFT stages
    do i = 0, int(log(real(n)) / log(2.0)) - 1
        n1 = n2
        n2 = n2 * 2
        u = (1.0, 0.0)  ! Initialize the twiddle factor for each stage
        
        ! Calculate twiddle factors for the current stage
        t = exp(cmplx(0.0, -2 * pi / real(n2)))
 
        ! Inner loop over elements of the array
        do j = 0, n1 - 1
            ! Update the twiddle factor for each sub FFT
            u = exp(cmplx(0.0, -2 * pi * real(j) / real(n2)))
            do k = j, n - 1, n2
                ! Apply FFT on pairs of elements
                t = u * x(k + n1 + 1)
                x(k + n1 + 1) = x(k + 1) - t
                x(k + 1) = x(k + 1) + t
            end do
        end do
    end do
 end subroutine fft
 
 
 
 end program test_fft_box_function
 