! chk_literal_too_precise.f90 --
!     Check if the compiler warns about constants with too many decimals
!
program chk_literal_too_precise
    implicit none

    real, parameter :: pi = 3.14159265358979323846
    real            :: x

    write(*,*) 'This program sets single-precision variables to very precise values'

    x = 0.12345678901234567890

    write(*,*) 'X:  ', x
    write(*,*) 'Pi: ', pi

end program chk_literal_too_precise
