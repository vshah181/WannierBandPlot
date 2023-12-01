subroutine make_kpath(nkpath, high_sym_pts, nkpt_per_path, nkp, kp, kdists)
implicit none
    integer, intent(in) :: nkpath, nkpt_per_path, nkp
    real*8, intent(in) :: high_sym_pts(nkpath+1,3)
    real*8, intent(out) :: kp(nkp, 3), kdists(nkp)
    integer :: ik, i, j
    real*8 :: kpath(3), dk(3)

    ik=1
    kdists(1) = 0d0
    do i=1, nkpath
        kpath=high_sym_pts(i+1, :)-high_sym_pts(i, :)
        dk=kpath/nkpt_per_path
        do j=1, nkpt_per_path
            kp(ik, :)=high_sym_pts(i, :)+(j-1)*dk
            kdists(ik+1)=norm2(dk)+kdists(ik)
            ik=ik+1
        end do
    end do
    kp(nkp, :)=high_sym_pts(nkpath+1, :)

end subroutine make_kpath
