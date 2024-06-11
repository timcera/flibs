! chk_unset_intent_out.f90 --
!     Check if the compiler warns about using an intent(out) argument before it is set
!
program chk_unset_intent_out
    implicit none

    integer :: x

    call add( x )

contains
subroutine add( y )
    integer, intent(out) :: y

    y = y + 2
end subroutine add
end program chk_unset_intent_out
