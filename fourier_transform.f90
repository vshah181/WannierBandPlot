subroutine ft_ham_r(num_bands, k, k_ham, r_list, weights, r_ham_list, num_r_pts)
use constants, only : tau
use, intrinsic :: iso_fortran_env, only: real64
implicit none
    integer, intent(in) :: num_r_pts, num_bands
    integer, intent(in) :: r_list(num_r_pts, 3), weights(num_r_pts)
    complex (real64), intent(out) :: k_ham(num_bands, num_bands)
    complex (real64), intent(in) :: r_ham_list(num_r_pts, num_bands, num_bands)
    complex (real64) :: r_ham(num_bands, num_bands)
    real (real64) :: k(3), phase
    integer :: ir, w

    k_ham=0d0
    do ir=1, num_r_pts
        w=weights(ir)
        phase=dot_product(k*tau, r_list(ir, :))
        r_ham=r_ham_list(ir, :, :)
        k_ham=k_ham+(r_ham*cmplx(cos(phase), sin(phase), kind=real64)/w)
    end do
end subroutine ft_ham_r
