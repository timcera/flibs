! chk_assign_unallocated.f90 --
!     Check if the compiler warns about setting elements of unallocated arrays
!
program chk_assign_unallocated
    integer, dimension(:), allocatable :: x

    write(*,*) 'This program set an element of an unallocated array'
    write(*,*) 'This probably causes a run-time error'

    x(1) = 1

    write(*,*) 'X(1): ', x(1)

end program chk_assign_unallocated
