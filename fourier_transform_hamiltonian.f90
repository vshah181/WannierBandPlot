subroutine ft_ham_r(num_bands, k, k_ham, r_list, weights, &
    r_ham_list, num_r_pts)
use constants, only : pi, tau
implicit none
    complex*16, intent(out) :: k_ham(num_bands, num_bands)
    complex*16, intent(in) :: r_ham_list(num_r_pts, num_bands, num_bands)
    real*8, intent(in) :: weights(num_r_pts), k(3)
    real*8 :: phase
    integer*4, intent(in) :: r_list(num_r_pts, 3)
    integer*4 :: r(3)
    integer*4, intent(in) :: num_r_pts, num_bands
    integer*4 :: ir, il, irow, icol, ii, jj, ib, jb, j
    k_ham=0d0

    do ir=1, num_r_pts
        r=r_list(ir, :)
        phase=dot_product(tau*k, r)*(-1d0)
        k_ham=k_ham+(r_ham_list(ir,:,:)*dcmplx(dcos(phase),dsin(phase)))/weights(ir)
    end do
end subroutine ft_ham_r
