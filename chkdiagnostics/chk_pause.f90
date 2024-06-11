! chk_pause.f90 --
!     Check if the compiler warns about the use of the PAUSE statement
!
program chk_pause
    implicit none

    integer :: x = 0

    write(*,*) 'This program contains a PAUSE statement'

    if ( x > 1 ) then
        pause
    else
        write(*,*) 'The PAUSE statement is not actually run (to avoid the actual pause)'
    endif

end program chk_pause
