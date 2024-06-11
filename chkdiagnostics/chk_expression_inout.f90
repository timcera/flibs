! chk_expression_inout.f90 --
!     Check if the compiler/runtime warns about passing an expression as an int/out argument
!
program chk_expression_inout
    implicit none

    integer :: x

    call add( x+1 )

contains
subroutine add( y )
    integer, intent(inout) :: y

    y = y + 2
end subroutine add
end program chk_expression_inout
