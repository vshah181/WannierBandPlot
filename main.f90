program band_plot
use file_parsing
implicit none
    real*8, allocatable :: kp(:,:), kdists(:)
    integer :: nkp
    integer*4 :: info, lwork
    real*8, allocatable :: rwork(:), energies(:, :)
    complex*16, allocatable :: work(:), kham(:, :)

    call read_kpoints ! nkpath, high_sym_pts, nkpt_per_path
    nkp=1+(nkpt_per_path*nkpath)
    allocate(kp(nkp, 3), kdists(nkp))
    call make_kpath(nkpath, high_sym_pts, nkpt_per_path, nkp, kp, kdists)

    call read_hr ! r_list, r_ham_list, weights, num_r_pts, num_bands
    allocate(energies(nkp, num_bands), k_ham(num_bands, num_bands))

    lwork=max(1, 2*num_bands-1)
    allocate(work(max(1, lwork), rwork(max(1, 3*num_bands-2)))
    do ik=1, nkp
        call ft_ham_r(num_bands, kp(ik, :), k_ham, r_list, weights, r_ham_list,&
            num_r_pts)
        call zheev('V', 'L', num_bands, k_ham, num_bands, energies(ik, :),     &
            work, lwork, rwork, info)
    end do
end program band_plot
