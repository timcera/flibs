! chk_mixed_precision.f90 --
!     Check if the compiler warns about expressions with mixed precision
!
program chk_mixed_precision
    implicit none

    integer, parameter :: dp = kind(1.0d0)
    real               :: x
    real(kind=dp)      :: y, z

    write(*,*) 'This program uses an expression with mixed precision'

    x = 1.2
    y = 2.4

    z = x + y
    write(*,*) z

end program chk_mixed_precision
