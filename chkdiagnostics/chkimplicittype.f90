! chkimplicittype.f90 --
!     Check whether the compiler warns about implicit typing of variables
!
program chkimplicittype
    real, dimension(10) :: x
    !real                :: z

    call random_number( x )

    y = 2.0 * x(1)
    write(*,*) y, func(x)

contains
real function func(x)
    real, dimension(:) :: x
    real               :: z

    func = z + sum(x)
end function func
end program chkimplicittype
