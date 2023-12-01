program band_plot
use file_parsing
implicit none
    real*8, allocatable :: kp(:,:), kdists(:), hsym_kdists(:)
    integer :: nkp, ik
    integer*4 :: info, lwork
    real*8, allocatable :: rwork(:), energies(:, :)
    complex*16, allocatable :: work(:), kham(:, :)

    call read_kpoints ! nkpath, high_sym_pts, nkpt_per_path
    nkp=1+(nkpt_per_path*nkpath)
    allocate(kp(nkp, 3), kdists(nkp), hsym_kdists(1+nkpath))
    call make_kpath(nkpath, high_sym_pts, nkpt_per_path, nkp, kp, kdists,      &
        hsym_kdists)

    call read_hr ! r_list, r_ham_list, weights, num_r_pts, num_bands
    allocate(energies(nkp, num_bands), kham(num_bands, num_bands))

    lwork=max(1, 2*num_bands-1)
    allocate(work(max(1, lwork)), rwork(max(1, 3*num_bands-2)))
    do ik=1, nkp
        call ft_ham_r(num_bands, kp(ik, :), kham, r_list, weights, r_ham_list, &
            num_r_pts)
        call zheev('V', 'L', num_bands, kham, num_bands, energies(ik, :), work,&
            lwork, rwork, info)
    end do

    call write_bands(nkp, num_bands, kdists, energies)
    call write_gnuplot_file(nkpath, hsym_kdists)
end program band_plot
