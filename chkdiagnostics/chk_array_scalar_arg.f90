! chk_array_scalar_arg.f90 --
!     Check if the compiler warns about the mixing of array and scalar arguments
!     (a practice tha twas not uncommon with FORTRAN 77)
!
program chk_array_scalar_arg
    real, dimension(5) :: x

    write(*,*) 'This program uses arrays and scalars as arguments'

    x =  [0.0, 1.0, 2.0, 3.0, 4.0]

    call sub( x )
    call sub( x(1) )
contains

subroutine sub( y )
    real y(*)

    write(*,*) 'First element: ', y(1)
end subroutine sub
end program chk_array_scalar_arg
