! chk_unset_argument.f90 --
!     Check if the compiler warns about arguments that are not set
!
program chk_unset_argument
    implicit none

    real :: x, y, w

    write(*,*) 'This program uses a subroutine that does not set all intent(out) arguments (w)'

    x = 0.2

    call sub( x, w, y )
    write(*,*) 'Result: ', y

contains
subroutine sub( x, w, y )
    real, intent(in)  :: x
    real, intent(out) :: w, y

    y = x ** 2
end subroutine sub

end program chk_unset_argument
