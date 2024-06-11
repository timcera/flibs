! chk_unalloc_intent_out.f90 --
!     Check if the compiler warns about using an intent(out) allocatable array before it is allocated
!
program chk_unalloc_intent_out
    implicit none

    integer, dimension(:), allocatable :: x

    allocate( x(10) )
    call add( x )

contains
subroutine add( y )
    integer, dimension(:), allocatable, intent(out) :: y

    y = y + 2
end subroutine add
end program chk_unalloc_intent_out
