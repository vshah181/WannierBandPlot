module constants
implicit none
    real*8, parameter :: pi = 4d0*datan(1d0)
    real*8, parameter :: tau = 8d0*datan(1d0)
    real*8, parameter :: planck_constant = 6.62607015e-34
    real*8, parameter :: reduced_planck_constant = planck_constant/tau
    real*8, parameter :: elementary_charge = 1.602176634e-19
    real*8, parameter :: planck_constant_ev = planck_constant/elementary_charge
    real*8, parameter :: reduced_planck_constant_ev = reduced_planck_constant  &
        /elementary_charge
end module constants

