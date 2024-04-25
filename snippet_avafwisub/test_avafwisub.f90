PROGRAM TestAVAfwisub

    IMPLICIT NONE
    INTEGER, PARAMETER :: na=10
    INTEGER, PARAMETER :: nx=10
    ! Declaration of variables

    INTEGER :: dtin, na1, na2, nangout1, nangout2
    INTEGER :: napulso, nitex, chsis, chout
    REAL :: x1, dx, freqmax, famp, h, ermin, lambda
    REAL :: evp, evs, erho
    REAL :: realsis(na, nx), vp(na), vs(na), rho(na), wavelet(128)
    REAL, ALLOCATABLE :: sisout(:, :)
    
    ! Initializing variables
    
  
    dtin = 1
    x1 = 0.0
    dx = 0.1
    na1 = 1
    na2 = 5
    nangout1 = 1
    nangout2 = 3
    freqmax = 50.0
    famp = 1.0
    napulso = 1
    nitex = 10
    h = 0.1
    ermin = 0.01
    lambda = 0.005
    chsis = 1
    chout = 2
    evp = 1.0
    evs = 0.5
    erho = 2.3
    
    ! Allocating arrays
    ALLOCATE(sisout(na, nx))
    
    ! Initialize inputs
    realsis = 1.0  ! Example data
    wavelet = 0.0
    wavelet(64) = 1.0  ! Simple spike for the wavelet
    vp = 2.0
    vs = 1.0
    rho = 2.0
    
    ! Call the subroutine
    CALL avafwisub(na, dtin, nx, dx, na1, na2, x1, nangout1, nangout2, &
                   realsis, vp, vs, rho, wavelet, freqmax, famp, napulso, &
                   h, nitex, ermin, lambda, chsis, sisout, chout, evp, &
                   evs, erho)
        
    ! Output some of the results for verification
    PRINT *, 'Output VP:', vp
    PRINT *, 'Output VS:', vs
    PRINT *, 'Output RHO:', rho
    PRINT *, 'Output Seismic Data:', sisout(:, 1)  ! Print first trace
    
    ! Deallocate arrays
    DEALLOCATE(sisout)

END PROGRAM TestAVAfwisub