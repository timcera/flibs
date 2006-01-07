! automdiff.f90 --
!     Module for automatic differentation:
!
!     With this module you can define a function that, when
!     evaluated returns both the value and the first derivative. The
!     independent variable should be of type AUTODERIV, as should be the
!     result.
!
module automatic_differentiation
    use select_precision
    implicit none

    type AUTODERIV
        real(wp) :: v
        real(wp) :: dv
    end type AUTODERIV

    interface sin
        module procedure sin_v
    end interface

    interface cos
        module procedure cos_v
    end interface

    interface tan
        module procedure tan_v
    end interface

    interface exp
        module procedure exp_v
    end interface

    interface log
        module procedure log_v
    end interface

    interface sqrt
        module procedure sqrt_v
    end interface

    interface operator(+)
        module procedure add_cv
        module procedure add_vc
        module procedure add_vv
    end interface

    interface operator(-)
        module procedure sub_cv
        module procedure sub_vc
        module procedure sub_vv
        module procedure usub_v  ! Unary minus!
    end interface

    interface operator(*)
        module procedure mult_cv
        module procedure mult_vc
        module procedure mult_vv
    end interface

    interface operator(/)
        module procedure div_cv
        module procedure div_vc
        module procedure div_vv
    end interface

    interface operator(**)
        module procedure expon_cv
        module procedure expon_vc
        module procedure expon_vv
        module procedure expon_vn ! x ** n
    end interface

contains

! derivvar
!     Properly initialthe an independent variable
! Arguments:
!     val        Value for the variable
! Result:
!     Independent variable with value ""val" and dervative 1
!
function derivvar( val ) result( x )
    real(wp)        :: val
    type(AUTODERIV) :: x

    x%v  = val
    x%dv = 1.0_wp
end function derivvar

! sin, cos, tan, log, exp, sqrt
!     Return the sine etc. of x
! Arguments:
!     x          Variable in question
! Result:
!     The function value and its first derivative at x
! Note:
!     Variables (not constant) need to be initialised as
!     x = autoderiv(x0,1.0). Use the function derivvar
!     for convenience.
!
elemental function sin_v( x ) result( y )
    type(AUTODERIV), intent(in) :: x
    type(AUTODERIV)             :: y

    y%v  = sin(x%v)
    y%dv = x%dv * cos(x%v)
end function sin_v

elemental function cos_v( x ) result( y )
    type(AUTODERIV), intent(in) :: x
    type(AUTODERIV)             :: y

    y%v  = cos(x%v)
    y%dv = -x%dv * sin(x%v)
end function cos_v

elemental function tan_v( x ) result( y )
    type(AUTODERIV), intent(in) :: x
    type(AUTODERIV)             :: y

    y%v  = tan(x%v)
    y%dv = x%dv / (1.0 + y%v**2 )
end function tan_v

elemental function exp_v( x ) result( y )
    type(AUTODERIV), intent(in) :: x
    type(AUTODERIV)             :: y

    y%v  = exp(x%v)
    y%dv = x%dv * y%v
end function exp_v

elemental function log_v( x ) result( y )
    type(AUTODERIV), intent(in) :: x
    type(AUTODERIV)             :: y

    y%v  = log(x%v)
    y%dv = x%dv / x%v
end function log_v

elemental function sqrt_v( x ) result( y )
    type(AUTODERIV), intent(in) :: x
    type(AUTODERIV)             :: y

    y%v  = sqrt(x%v)
    y%dv = 0.5 *x%dv / y%v
end function sqrt_v

! +, -, *, /, **
!     Implementation of the arithmetic operations
! Arguments:
!     x          First operand (constant or variable)
!     y          Second operand (constant or variable)
! Result:
!     The sum, etc. and its first (total) derivative
! Note:
!     We need three versions to allow combinations
!     with constants.
!
elemental function add_vv( x, y ) result( z )
    type(AUTODERIV), intent(in) :: x
    type(AUTODERIV), intent(in) :: y
    type(AUTODERIV)             :: z

    z%v  = x%v  + y%v
    z%dv = x%dv + y%dv
end function add_vv

elemental function add_cv( x, y ) result( z )
    real(wp), intent(in)        :: x
    type(AUTODERIV), intent(in) :: y
    type(AUTODERIV)             :: z

    z%v  = x    + y%v
    z%dv = y%dv
end function add_cv

elemental function add_vc( x, y ) result( z )
    type(AUTODERIV), intent(in) :: x
    real(wp), intent(in)        :: y
    type(AUTODERIV)             :: z

    z%v  = x%v  + y
    z%dv = x%dv
end function add_vc

elemental function usub_v( x ) result( z )
    type(AUTODERIV), intent(in) :: x
    type(AUTODERIV)             :: z

    z%v  = -x%v
    z%dv = -x%dv
end function usub_v

elemental function sub_vv( x, y ) result( z )
    type(AUTODERIV), intent(in) :: x
    type(AUTODERIV), intent(in) :: y
    type(AUTODERIV)             :: z

    z%v  = x%v  - y%v
    z%dv = x%dv - y%dv
end function sub_vv

elemental function sub_cv( x, y ) result( z )
    real(wp), intent(in)        :: x
    type(AUTODERIV), intent(in) :: y
    type(AUTODERIV)             :: z

    z%v  = x  - y%v
    z%dv =    - y%dv
end function sub_cv

elemental function sub_vc( x, y ) result( z )
    type(AUTODERIV), intent(in) :: x
    real(wp), intent(in)        :: y
    type(AUTODERIV)             :: z

    z%v  = x%v  - y
    z%dv = x%dv
end function sub_vc

elemental function mult_vv( x, y ) result( z )
    type(AUTODERIV), intent(in) :: x
    type(AUTODERIV), intent(in) :: y
    type(AUTODERIV)             :: z

    z%v  = x%v  * y%v
    z%dv = x%dv * y%v + x%v * y%dv
end function mult_vv

elemental function mult_cv( x, y ) result( z )
    real(wp), intent(in)        :: x
    type(AUTODERIV), intent(in) :: y
    type(AUTODERIV)             :: z

    z%v  = x * y%v
    z%dv = x * y%dv
end function mult_cv

elemental function mult_vc( x, y ) result( z )
    type(AUTODERIV), intent(in) :: x
    real(wp), intent(in)        :: y
    type(AUTODERIV)             :: z

    z%v  = x%v  * y
    z%dv = x%dv * y
end function mult_vc

elemental function div_vv( x, y ) result( z )
    type(AUTODERIV), intent(in) :: x
    type(AUTODERIV), intent(in) :: y
    type(AUTODERIV)             :: z

    z%v  = x%v  / y%v
    z%dv = (x%dv * y%v - x%v * y%dv) / y%v ** 2
end function div_vv

elemental function div_cv( x, y ) result( z )
    real(wp), intent(in)        :: x
    type(AUTODERIV), intent(in) :: y
    type(AUTODERIV)             :: z

    z%v  = x / y%v
    z%dv = -x * y%dv / y%v ** 2
end function div_cv

elemental function div_vc( x, y ) result( z )
    type(AUTODERIV), intent(in) :: x
    real(wp), intent(in)        :: y
    type(AUTODERIV)             :: z

    z%v  = x%v  / y
    z%dv = x%dv / y
end function div_vc

elemental function expon_vv( x, y ) result( z )
    type(AUTODERIV), intent(in) :: x
    type(AUTODERIV), intent(in) :: y
    type(AUTODERIV)             :: z

    z%v  = x%v  ** y%v
    z%dv = (y%dv * log(x%v) + y%v/x%v * x%dv ) * z%v
end function expon_vv

elemental function expon_cv( x, y ) result( z )
    real(wp), intent(in)        :: x
    type(AUTODERIV), intent(in) :: y
    type(AUTODERIV)             :: z

    z%v  = x ** y%v
    z%dv = y%dv * log(x) * z%v
end function expon_cv

elemental function expon_vc( x, y ) result( z )
    type(AUTODERIV), intent(in) :: x
    real(wp), intent(in)        :: y
    type(AUTODERIV)             :: z

    z%v  = x%v  ** y
    z%dv = y/x%v * x%dv * z%v
end function expon_vc

elemental function expon_vn( x, n ) result( z )
    type(AUTODERIV), intent(in) :: x
    integer, intent(in)         :: n
    type(AUTODERIV)             :: z

    z%v  =            x%v ** n
    z%dv = n * x%dv * x%v ** (n-1)
end function expon_vn

end module automatic_differentiation
