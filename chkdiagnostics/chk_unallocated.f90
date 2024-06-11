! chk_unallocated.f90 --
!     Check if the compiler warns about using unallocated arrays
!
program chk_unallocated
    integer, dimension(:), allocatable :: x

    write(*,*) 'This program uses an unallocated array'
    write(*,*) 'This probably causes a run-time error'

    x = 1

    write(*,*) 'X: ', x

end program chk_unallocated
