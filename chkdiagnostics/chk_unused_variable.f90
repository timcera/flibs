! chk_unused_variable.f90 --
!     Check if the compiler warns about variables that are not used
!
program chk_unused_variable
    implicit none

    real :: x, y, w

    write(*,*) 'This program declares some variables that are not used (w and z)'

    x = 0.2
    y = func( x )

    write(*,*) 'Result: ', y

contains
real function func( x )
    real, intent(in) :: x

    real             :: z

    func = x ** 2
end function func

end program chk_unused_variable
