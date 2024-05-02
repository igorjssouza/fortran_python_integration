! avafwisub_interface.f90
subroutine avafwisub(na, dtin, nx, dx, na1, na2, x1, nangout1, nangout2, &
  realsis, vp, vs, rho, wavelet, freqmax, famp, napulso, &
  h, nitex, ermin, lambda, chsis, sisout, chout, evp, &
  evs, erho)
implicit none
integer, intent(in) :: na, nx, na1, na2, nangout1, nangout2
integer, intent(in) :: napulso, nitex, chsis, chout
real, intent(in) :: dtin, x1, dx, freqmax, famp, h, ermin, lambda
real, intent(in) :: evp, evs, erho
real, intent(in) :: realsis(na, nx), wavelet(128)
real, intent(inout) :: vp(na), vs(na), rho(na)
real, dimension(na, nx), intent(out) :: sisout
end subroutine avafwisub