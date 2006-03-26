! test_diff.f90 --
!     Test (not exhausitvely, alas) the module for automatic
!     differentation
!
!     Define a non-trivial function and show that we can
!     evaluate both the value and the first derivative without having to
!     go through mathematics ourselves.
!
program test_diff
    use automatic_differentiation
    implicit none

    type(AUTODERIV)  :: x
    integer          :: i

    do i = 1,20
        x = derivvar( 0.2 * (i-1) )
        write( *, * ) f(x), df(x%v)
    enddo

contains
!
! The AUTODERIV function and a function that implements the
! first derivative directly.
!
type(AUTODERIV) function f( x )
    type(AUTODERIV), intent(in)  :: x

    f = x ** 3 + 2.0 * x ** 2 - x + 3.0
end function f

real(wp) function df( x )
    real(wp) :: x

    df = 3.0 * x ** 2 + 4.0 * x  - 1.0
end function df

end program
