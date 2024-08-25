module constants
use, intrinsic :: iso_fortran_env, only: real64
implicit none
    real (real64), parameter :: pi=4d0*datan(1d0)
    real (real64), parameter :: tau=8d0*datan(1d0)
    real (real64), parameter :: planck_constant=6.62607015e-34
    real (real64), parameter :: reduced_planck_constant=planck_constant/tau
    real (real64), parameter :: elementary_charge=1.602176634e-19
    real (real64), parameter :: planck_constant_ev                             &
        =planck_constant/elementary_charge
    real (real64), parameter :: reduced_planck_constant_ev                     &
        =reduced_planck_constant/elementary_charge
end module constants

