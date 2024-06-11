! chk_long_line_trunc.f90 --
!     Check if the compiler warns about code beyond position 132
!
program chk_long_line_trunc
    implicit none

    integer :: x, y

    write(*,*) 'The source of this program contains a line longer than 132'
    write(*,*) 'If the compiler does not allow that, then it may or may not complain about it'

    x = 1
    y = x                                                                                                                           + 1

    write(*,*) 'y? ', y

    if ( y > 1 ) then
        write(*,*) 'The compiler has used the complete line in the source code'
    else
        write(*,*) 'The compiler has truncated the statement to calculate y'
    endif

end program chk_long_line_trunc
