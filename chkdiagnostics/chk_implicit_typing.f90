! chk_implicit_typing.f90 --
!     Check whether the compiler warns about implicit typing of variables
!
program chk_implicit_typing
    real, dimension(10) :: x

    write(*,*) 'This program has two undeclared variables - y and z'

    call random_number( x )

    y = 2.0 * x(1)             ! y is not explicitly declared
    write(*,*) y, func(x)

contains
real function func(x)
    real, dimension(:) :: x

    func = z + sum(x)          ! z is not explicitly declared
end function func
end program chk_implicit_typing
