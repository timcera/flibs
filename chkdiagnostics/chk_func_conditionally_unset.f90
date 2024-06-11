! chk_func_conditionally_unset.f90 --
!     Check if the compiler warns about functions that do not set a value
!
program chk_func_conditionally_unset
    implicit none

    real :: x, y

    write(*,*) 'This program calls a function that does not always set a value'

    x = 0.2
    y = func( x )

    write(*,*) 'Result: ', y

contains
real function func( x )
    real, intent(in) :: x

    if ( x > 0.3 ) then
        func = x ** 2
    endif
end function func

end program chk_func_conditionally_unset
