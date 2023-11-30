subroutine ft_ham_r(num_bands, k, k_ham, r_list, weights, r_ham_list, num_r_pts)
use constants, only : tau
implicit none
    complex*16, intent(out) :: k_ham(num_bands, num_bands)
    complex*16, intent(in) :: r_ham_list(num_r_pts, num_bands, num_bands)
    complex*16 :: r_ham(num_bands, num_bands)
    real*8 :: k(3), phase
    integer, intent(in) :: r_list(num_r_pts, 3), num_r_pts, num_bands,         &
        weights(num_r_pts)
    integer :: ir, w

    k_ham=0d0
    do ir=1, num_r_pts
        w=weights(ir)
        phase=dot_product(k*tau, r_list(ir, :))
        r_ham=r_ham_list(ir, :, :)
        k_ham = k_ham + (r_ham * dcmplx(dcos(phase), -dsin(phase)) / w)
    end do
end subroutine ft_ham_r
