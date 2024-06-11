! chk_func_unset.f90 --
!     Check if the compiler warns about functions that do not set a value
!
program chk_func_unset
    implicit none

    real :: x, y

    write(*,*) 'This program calls a function that does not set a value'

    x = 0.2
    y = func( x )

    write(*,*) 'Result: ', y

contains
real function func( x )
    real, intent(in) :: x

    real             :: z

    z = x ** 2
end function func

end program chk_func_unset
