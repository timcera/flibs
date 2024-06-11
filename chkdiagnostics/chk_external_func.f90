! chk_external_func.f90 --
!     Check if the compiler warns about external functions with an implicit interface
!
program chk_external_func
    real, dimension(5) :: x
    real, external     :: func

    write(*,*) 'This program uses a function that is declared as external'

    x = [0.0, 1.0, 2.0, 3.0, 4.0]

    write(*,*) func(x)
end program chk_external_func

real function func( y )
    real y(*)

    func = y(1)
end function func
