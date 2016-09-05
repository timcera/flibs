! chk_norm2_hypot.f90 --
!     Check if the compiler supports the new NORM2 and HYPOT functions
!
!     Note: there is a whole list of such special functions defined for the Fortran 2008 standard, not
!     all of them are tested here
!
program chk_norm2_hypot
    implicit none

    real, dimension(4) :: vector = (/ 1.0, 2.0, 3.0, 4.0 /)
    real               :: x = 3.0, y = 4.0

    write( *, '(a)' )              'HYPOT function - the (3,4,5) triangle'
    write( *, '(a,2f7.4,a,f7.4)' ) 'The hypothenusa of a rectangular triangle with sides', x, y, ' is ', hypot(x,y)

    write( *, '(a)' )              '2-norm of a vector:'
    write( *, '(a,f10.4)' )        'Euclidean norm of a vector:', norm2(vector)
end program chk_norm2_hypot
