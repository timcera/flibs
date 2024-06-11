! chk_pointer_freed_alloc.f90 --
!     Check if the compiler/runtime warns about assigning a pointer to (clearly) deallocated memory
!
program chk_pointer_freed_alloc
    implicit none

    integer, dimension(:), allocatable, target :: x
    integer, dimension(:), pointer             :: px

    allocate( x(10) )

    x = 1

    deallocate( x )

    write(*,*) 'Assigning to the deallocated array ...'

    px => x

    write(*,*) 'Contents (assignment presented no problem): '
    write(*,*) px
end program chk_pointer_freed_alloc
