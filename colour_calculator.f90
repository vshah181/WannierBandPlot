module colour_calculation
implicit none
private
public get_colours
contains
    subroutine get_colours(num_bands, n_orb, k_ham, colours)
        integer, intent(in) :: num_bands, n_orb
        complex*16, intent(in) :: k_ham
        real*8, intent(out) :: colours(3, num_bands)
        complex*16 :: eigenvector(num_bands), miniket(n_orb), 
        real*8 :: hue_step, hues(n_orb), minivector(norb), element, hsl(3)
        integer :: ib, ih, io, jo
        colours = 0d0
        
        hue_step=360.0/norb
        do ih=1, norb
            hues(ih)=hue_step*(ih-1)
        end do
        do ib=1, num_bands
            eigenvector=k_ham(:, ib)
            hue=0d0
            do io=1, num_bands, n_orb
                miniket = eigenvector(io:io+n_orb-1)
                do jo=1, n_orb
                    element=miniket(jo)*dconjg(miniket(jo))
                    minivector(jo)=real(element)
                end do
                hue=hue+dot_product(hues, minivector)
            end do
            hsl=(/ hue, 1d0, 0.5d0 /)
            call hsl_to_rgb(hsl, colours(:, ib))
        end do
    end subroutine get_colours

    subroutine hsl_to_rgb(hsl, rgb)
        real*8, intent(in) :: hsl
        real*8, intent(out) :: rgb
        real*8 :: chroma, hp, x, rgb_p(3), m

        chroma=(1-dabs(2*hsl(3)-1))*hsl(2)
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
        rgb(1)=rgb_p(1)+m
        rgb(2)=rgb_p(2)+m
        rgb(3)=rgb_p(3)+m

    end subroutine hsl_to_rgb
end module colour_calculation
