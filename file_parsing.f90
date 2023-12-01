module file_parsing
implicit none
private
    character(len=22), parameter :: hamiltonian="SrTiO3_hr.dat"
    character(len=22), parameter :: kpt_file="kpoints"
    complex*16, allocatable :: r_ham_list(:, :, :)
    real*8, allocatable :: high_sym_pts(:, :)
    integer, allocatable :: r_list(:, :), weights(:)
    character, allocatable :: high_sym_pt_symbols(:)
    integer :: num_bands, num_r_pts, nkpt_per_path, nkpath
    public num_bands, num_r_pts, weights, r_list, r_ham_list, read_hr,         &
           write_bands, high_sym_pts, nkpath, nkpt_per_path, read_kpoints,     &
           write_gnuplot_file
contains
    subroutine read_hr
        integer :: hi_row, hi_col, ir, o_i, o_j
        real*8 :: rp, ip

        open(101, file=hamiltonian)
        read(101, *)
        read(101, *)num_bands, num_r_pts  ! bands and number of real-points
        allocate(weights(num_r_pts), r_list(num_r_pts, 3),                     &
                 r_ham_list(num_r_pts, num_bands, num_bands))
        read(101, *)weights ! degeneracy of each Wigner-Seitz grid point
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
        allocate(high_sym_pts(nkpath, 3), high_sym_pt_symbols(nkpath))
        do i=1, nkpath
            read(102, *) high_sym_pts(i, :), high_sym_pt_symbols(i)
        end do
        close(102)
        nkpath=nkpath-1
    end subroutine read_kpoints

    subroutine write_bands(nkp, num_bands, kdists, energies, colours)
        integer, intent(in) :: nkp, num_bands
        real*8, intent(in) :: kdists(nkp), energies(nkp, num_bands),           &
            colours(3, nkp, num_bands)
        logical :: file_exist
        character(len=22) :: ofname='wannier_band.dat'
        integer :: ik, ib, red, green, blue

        inquire(file=ofname, exist=file_exist) 
        if (file_exist) then 
            open(201, file=ofname, action='write', status='replace')
        else
            open(201, file=ofname, status='new')
        endif

        do ib=1, num_bands
            if(ib .ne. 1)  write(201, fmt='(a)') ' '
            do ik=1, nkp
                red=nint(colours(1, ik, ib))
                green=nint(colours(2, ik, ib))
                blue=nint(colours(3, ik, ib))
                write(201, fmt='(2f12.7,3i5)') kdists(ik), energies(ik, ib),   &
                    red, green, blue
            end do
        end do
        close(201)
    end subroutine write_bands

    subroutine write_gnuplot_file(nkpath, hsym_kdists)
        integer, intent(in) :: nkpath
        real*8, intent(in) :: hsym_kdists(nkpath+1)
        character(len=22) :: ofname='wannier_band.gnu', hsym_char
        logical :: file_exist
        integer :: it


        inquire(file=ofname, exist=file_exist) 
        if (file_exist) then 
            open(202, file=ofname, action='write', status='replace')
        else
            open(202, file=ofname, status='new')
        endif

        write(202, fmt='(a)') 'set encoding iso_8859_1'
        write(202, fmt='(a)', advance='no') 'set terminal pdfcairo enhanced'
        write(202, fmt='(4a)', advance='no') ' font ', '"', 'Arial', '"'
        write(202, fmt='(a)') ' transparent size 17.8cm, 12.7cm'                
        write(202, fmt='(4a)') 'set output ', '"', 'wannier_band.pdf', '"'
        write(202, fmt='(a)', advance='no') 'set xtics('
        do it=1, nkpath+1
            hsym_char=high_sym_pt_symbols(it)
            if (trim(adjustl(hsym_char)) .eq. 'G') hsym_char = '{/Symbol \107}'
            if (it .ne. nkpath+1) then 
                write(202, fmt='(3a,1x,f10.7,a,1x)', advance='no') '"',        &
                    trim(adjustl(hsym_char)), '"', hsym_kdists(it), ','
            else 
                write(202, fmt='(3a,1x,f10.7,a)') '"',                         &
                    trim(adjustl(hsym_char)), '"', hsym_kdists(it), ')'
            endif
        end do
        write(202, fmt='(5a)') 'set style line 10 lt 1 lc rgb ', '"', 'black', &
            '"', ' lw 1'
        write(202, fmt='(a)') 'set border ls 10'
        write(202, fmt='(4a)') 'set tics textcolor rgb ', '"','black','"'
        write(202, fmt='(4a)') 'set ylabel ', '"','{/:Italic E} (eV)', '"'
        write(202, fmt='(a)') 'unset key'
        write(202, fmt='(a)') 'rgb(r,g,b) = int(r)*65536 + int(g)*256 + int(b)'
        do it=1, nkpath+1
            write(202, fmt='(a,f10.7,a,f10.7,a)') 'set arrow from ',           &
            hsym_kdists(it), ', graph(0,0) to ', hsym_kdists(it),              &
            ', graph(1,1) nohead'
        end do
        write(202, fmt='(5a)') 'plot ', '"', 'wannier_band.dat', '"',          &
            'using 1:2:(rgb($3,$4,$5)) with line lw 2.0 lc rgb variable' 
        close(202)
    end subroutine write_gnuplot_file

end module file_parsing
