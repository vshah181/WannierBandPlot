subroutine make_kpath(nkpath, high_sym_pts, nkpt_per_path, nkp, kp, kdists,    &
        hsym_kdists)
use file_parsing, only : bvec
implicit none
    integer, intent(in) :: nkpath, nkpt_per_path, nkp
    real*8, intent(in) :: high_sym_pts(nkpath+1,3) 
    real*8 :: abs_hsym_pts(nkpath+1, 3)
    real*8, intent(out) :: kp(nkp, 3), kdists(nkp), hsym_kdists(nkpath+1)
    integer :: ik, i, j
    real*8 :: kpath(3), dk(3), abs_dk(3), abs_kpath(3)

    do i=1, 3
    end do
    do i=1, nkpath+1
        abs_hsym_pts(i, :) = high_sym_pts(i, 1) * bvec(1, :)                   &
                           + high_sym_pts(i, 2) * bvec(2, :)                   &
                           + high_sym_pts(i, 3) * bvec(3, :)
    end do
    ik=1
    kdists(1) = 0d0
    do i=1, nkpath
        kpath=high_sym_pts(i+1, :)-high_sym_pts(i, :)
        dk=kpath/nkpt_per_path
        abs_kpath=abs_hsym_pts(i+1, :)-abs_hsym_pts(i, :)
        abs_dk=abs_kpath/nkpt_per_path
        do j=1, nkpt_per_path
            kp(ik, :)=high_sym_pts(i, :)+(j-1)*dk
            kdists(ik+1)=norm2(abs_dk)+kdists(ik)
            ik=ik+1
        end do
    end do
    kp(nkp, :)=high_sym_pts(nkpath+1, :)

    do i=1, nkpath+1
        hsym_kdists(i)=kdists((nkpt_per_path*(i-1))+1)
    end do

end subroutine make_kpath
