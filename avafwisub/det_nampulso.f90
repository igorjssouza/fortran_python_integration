program avafwi
   implicit none

   integer :: nampulso, i, j
   real :: sispulso(128)
   real :: ep1, ep2, dt, fm
   complex :: wlet(128), amp(128)
   character*40 :: nomepulso

   nomepulso = 'wavelet-exp1.dat'
   dt = 0.004

   ! Reading seismic pulse
   open(14, file=nomepulso, status='old')
   do j=1, 128
      read(14, *) sispulso(j)
   end do
   close(unit=14)

   ! Determining the effective duration of the pulse
   ep1 = 0.0
   do i=1, 128
      ep1 = ep1 + sispulso(i)**2
   end do
   ep2 = sispulso(65)**2
   i = 0
   do while (ep2 / ep1 < 0.9999)
      i = i + 1
      ep2 = ep2 + sispulso(65+i)**2 + sispulso(65-i)**2
   end do
   nampulso = 2 * i + 1
   print *, ' '
   print *, ' '
   print *, 'Duração do pulso sísmico (num amostras): ', nampulso
   print *, 'Duracao do pulso sísmico (mseg.): ', dt * nampulso * 1000

   ! Obtaining seismic pulse amplitude spectrum
   do j=1, 128
      wlet(j) = cmplx(sispulso(j), 0.0)
   end do

   call fft(wlet, 128)

   ep1 = 0.0
   do j=1, 128
      amp(j) = sqrt(real(wlet(j))**2 + aimag(wlet(j))**2)
      ep1 = ep1 + amp(j)**2
   end do
   ep2 = amp(1)**2
   j = 1
   do while (ep2 / ep1 < 0.999999)
      j = j + 1
      ep2 = ep2 + amp(j)**2
   end do
   fm = j * 1.0 / (128 * dt)
   print *, 'Frequencia maxima do pulso sísmico (Hz): ', fm
   print *, ' '

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



end program avafwi
