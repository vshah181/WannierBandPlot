module file_parsing
implicit none
private
    character(len=22), parameter :: hamiltonian="KTaO3_VASP_hr.dat"
    character(len=22), parameter :: kpt_file="kpoints"
    complex*16, allocatable :: r_ham_list(:, :, :)
    real*8, allocatable :: high_sym_pts(:, :)
    integer, allocatable :: r_list(:, :), weights(:)
    integer :: num_bands, num_r_pts, nkpt_per_path, nkpath
    public num_bands, num_r_pts, weights, r_list, r_ham_list, read_hr,         &
           write_bands, high_sym_pts, nkpath, nkpt_per_path, read_kpoints
contains
    subroutine read_hr
        integer :: hi_row, hi_col, ir, o_i, o_j
        real*8 :: rp, ip

        open(101, file=hamiltonian)
        read(101, *)
        read(101, *)num_bands, num_r_pts  ! bands and number of real-points
        allocate(weights(num_r_pts), r_list(num_r_pts, 3),                     &
                 r_ham_list(num_r_pts, num_bands, num_bands))
        read(1, *)weights ! degeneracy of each Wigner-Seitz grid point
        do ir=1, num_r_pts
            do o_i=1, num_bands
                do o_j=1, num_bands
                    read(101, *)r_list(ir, 1), r_list(ir, 2), r_list(ir, 3),   &
                              hi_row, hi_col, rp, ip
                    r_ham_list(ir, hi_row, hi_col)=dcmplx(rp, ip)  
                end do
            end do
        end do
        close(101)
    end subroutine read_hr

    subroutine read_kpoints
        integer :: i
        open(102, file=kpt_file)
        read(102, *) nkpt_per_path
        read(102, *) nkpath
        allocate(high_sym_pts(nkpath, 3))
        do i=1, nkpath
            read(102, *) high_sym_pts(i, :)
        end do
        close(102)
        nkpath=nkpath-1
    end subroutine read_kpoints

    subroutine write_bands(nkp, num_bands, kdists, energies)
        integer, intent(in) :: nkp, num_bands
        real*8, intent(in) :: kdists(nkp), energies(nkp, num_bands)
        logical :: file_exist
        character(len=22) :: ofname='band.dat'
        integer :: ik, ib

        inquire(file=ofname, exist=file_exist) 
        if (file_exist) then 
            open(103, file=ofname, action='write', status='replace')
        else
            open(103, file=ofname, status='new')
        endif

        do ib=1, num_bands
            if(ib .gt. 1)  write(103, fmt='(a)') ' '
            do ik=1, nkp
                write(103, fmt='(2f12.7)') kdists(ik), energies(ik, ib)
            end do
        end do
        close(103)
    end subroutine write_bands
end module file_parsing
