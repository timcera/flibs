! chk_real_equals.f90 --
!     Check if the compiler warns about direct comparisons between real variables
!
program chk_real_equals
    implicit none

    integer, parameter :: dp = kind(1.0d0)
    real               :: xs, ys
    real(kind=dp)      :: xd, yd

    write(*,*) 'This program compares reals directly'

    xs = 0.2
    call random_number( ys )

    xd = 0.2_dp
    call random_number( yd )

    write(*,*) 'Single precision: ', xs == ys
    write(*,*) 'Double precision: ', xd == yd

end program chk_real_equals
