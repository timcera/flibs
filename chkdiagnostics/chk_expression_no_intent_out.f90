! chk_expression_no_intent_out.f90 --
!     Check if the compiler/runtime warns about passing an expression that will be changed
!
!     This may very well be acceptable
!
program chk_expression_no_intent_out
    implicit none

    integer :: x

    x = 1
    call add( x+1 )

contains
subroutine add( y )
    integer :: y

    y = y + 2
end subroutine add
end program chk_expression_no_intent_out
