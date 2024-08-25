module colour_calculation
use, intrinsic :: iso_fortran_env, only: real64
use file_parsing, only : basis
implicit none
private
public get_colours
contains
    subroutine get_colours(num_bands, n_orb, k_ham, colours)
        integer, intent(in) :: num_bands, n_orb
        complex (real64), intent(in) :: k_ham(num_bands, num_bands)
        real (real64), intent(out) :: colours(3, num_bands)
        complex (real64) :: eigenvector(num_bands), miniket(n_orb)
        real (real64) :: hue_step, hues(n_orb), element, hsl(3),   &
            base_colours(3, n_orb)
        integer :: ib, ih, io, jo
        colours = 0d0
        
        hue_step=360.0/n_orb
        do ih=1, n_orb
            hues(ih)=hue_step*(ih-1)
            hsl=(/ hues(ih), 1d0, 0.5d0 /)
            call hsl_to_rgb(hsl, base_colours(:, ih))
        end do
        if(trim(adjustl(basis)) .eq. 'uudd') then 
            do ib=1, num_bands
                eigenvector=k_ham(:, ib)
                do io=1, num_bands, n_orb
                    miniket = eigenvector(io:io+n_orb-1)
                    do jo=1, n_orb
                        element=real(miniket(jo)*conjg(miniket(jo)))
                        colours(:, ib)=colours(:, ib)                          &
                        +(real(element)*base_colours(:, jo))
                    end do
                end do
            end do
        elseif(trim(adjustl(basis)) .eq. 'udud') then
            do ib=1, num_bands
                eigenvector=k_ham(:, ib)
                do io=1, num_bands, 2
                    miniket((io+1)/2) = conjg(eigenvector(io))*eigenvector(io)&
                        +conjg(eigenvector(io+1))*eigenvector(io+1)
                end do
                do jo=1, n_orb
                    colours(:, ib)=colours(:, ib)                              &
                        +(real(miniket(jo))*base_colours(:, jo))
                end do
            end do
        end if
    end subroutine get_colours

    subroutine hsl_to_rgb(hsl, rgb)
        real (real64), intent(in) :: hsl(3)
        real (real64), intent(out) :: rgb(3)
        real (real64) :: chroma, hp, x, rgb_p(3), m

        chroma=(1-dabs(2*hsl(3)-1))*hsl(2)
        rgb_p = 0d0
        hp=hsl(1)/60
        x=chroma*(1-dabs(dmod(hp, 2d0)-1))
        if((0d0 .le. hp) .and. (hp .lt. 1d0)) then
            rgb_p = (/ chroma, x, 0d0 /)
        else if((1d0 .le. hp) .and. (hp .lt. 2d0)) then
            rgb_p = (/ x, chroma, 0d0 /)
        else if((2d0 .le. hp) .and. (hp .lt. 3d0)) then
            rgb_p = (/ 0d0, chroma, x /)
        else if((3d0 .le. hp) .and. (hp .lt. 4d0)) then
            rgb_p = (/ 0d0, x, chroma /)
        else if((4d0 .le. hp) .and. (hp .lt. 5d0)) then
            rgb_p = (/ x, 0d0, chroma /)
        else if((5d0 .le. hp) .and. (hp .le. 6d0)) then 
            rgb_p = (/ chroma, 0d0, x /)
        end if

        m=hsl(3)-(0.5d0*chroma)
        rgb(1)=(rgb_p(1)+m)*255d0
        rgb(2)=(rgb_p(2)+m)*255d0
        rgb(3)=(rgb_p(3)+m)*255d0
    end subroutine hsl_to_rgb
end module colour_calculation
