! chk_unassociated.f90 --
!     Check if the compiler warns about using unassociated arrays
!
program chk_unassociated
    integer, dimension(:), pointer :: x

    write(*,*) 'This program uses an unassociated array'
    write(*,*) 'This probably causes a run-time error'

    x = 1

    write(*,*) 'X: ', x

end program chk_unassociated
