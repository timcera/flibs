! chk_alloc_pointer_confusion.f90 --
!     Check if the compiler warns about confusing allocatables and pointers
!
program chk_alloc_pointer_confusion
    implicit none

    integer, allocatable, dimension(:), target :: x
    integer, pointer, dimension(:)             :: y

    write(*,*) 'This program allocates one array and deallocates it via a pointer'

    allocate( x(10) )
    y => x

    deallocate( y )

    write(*,*) 'x allocated?',  allocated(x)
    write(*,*) 'y associated?', associated(y)

    x = 1
    write(*,*) 'x: ', x

end program chk_alloc_pointer_confusion
