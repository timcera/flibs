! chk_unused_argument.f90 --
!     Check if the compiler warns about arguments that are not used
!
program chk_unused_argument
    implicit none

    real :: x, y, w, z

    write(*,*) 'This program uses a function and a subroutine that do not use some arguments (w and z)'

    x = 0.2
    y = func( x, w, z )

    write(*,*) 'Result: ', y

    call sub( x, w, z, y )
    write(*,*) 'Result: ', y

contains
real function func( x, w, z )
    real, intent(in) :: x, w, z

    func = x ** 2
end function func

subroutine sub( x, w, z, y )
    real, intent(in)  :: x, w, z
    real, intent(out) :: y

    y = x ** 2
end subroutine sub

end program chk_unused_argument
