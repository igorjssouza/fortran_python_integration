SUBROUTINE avafwisub(na, dtin, nx, dx, na1, na2, x1, nangout1, nangout2, &
  realsis, vp, vs, rho, wavelet, freqmax, famp, napulso, &
  h, nitex, ermin, lambda, chsis, sisout, chout, evp, &
  evs, erho)
IMPLICIT NONE
INTEGER, INTENT(IN) :: na, nx, na1, na2, nangout1, nangout2
INTEGER, INTENT(IN) :: napulso, nitex, chsis, chout
REAL, INTENT(IN) :: dtin, x1, dx, freqmax, famp, h, ermin, lambda
REAL, INTENT(IN) :: evp, evs, erho
REAL, INTENT(IN) :: realsis(na, nx), wavelet(128)
REAL, INTENT(INOUT) :: vp(na), vs(na), rho(na)
REAL, DIMENSION(na, nx), INTENT(OUT) :: sisout
ENDSUBROUTINE avafwisub