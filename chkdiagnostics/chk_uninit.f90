! chk_uninit.f90 --
!     Check whether the compiler warns about unitialised variables
!
program chk_uninit
    implicit none
    real :: z

    write(*,*) 'This program writes out a variable z that was not initialised'

    write(*,*) z  ! Clearly an uninitialised variable

end program chk_uninit
