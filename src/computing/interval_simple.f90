! interval_simple.f90 --
!     Straightforward implementation of interval arithmetic:
!     no dependency analysis
!
!     TODO:
!     Make sure rounding has the right effect!
!
!     $Id$
!
module interval_arithmetic
    use select_precision
    implicit none

    type INTERVAL
        real(kind=wp) :: lower
        real(kind=wp) :: upper
    end type INTERVAL

    interface operator(+)
        module procedure add
    end interface

    interface operator(-)
        module procedure sub
    end interface

    interface operator(*)
        module procedure mult
    end interface

    interface operator(/)
        module procedure div
    end interface

contains

! intval --
!     Create an interval number
! Arguments:
!     min     Lower bound of the interval
!     max     Upper bound of the interval
! Result:
!     New interval number
!
function intval( min, max ) result(r)
    real(kind=wp), intent(in) :: min, max
    type(INTERVAL)            :: r

    r%lower = min
    r%upper = max
end function intval

! add --
!     Add two interval numbers
! Arguments:
!     x        First interval number
!     y        Second interval number
! Result:
!     x+y
!
function add( x, y ) result(r)
    type(INTERVAL), intent(in) :: x, y
    type(INTERVAL)             :: r

    r%lower = x%lower + y%lower
    r%upper = x%upper + y%upper
end function add

! sub --
!     Subtract two interval numbers
! Arguments:
!     x        First interval number
!     y        Second interval number
! Result:
!     x-y
!
function sub( x, y ) result(r)
    type(INTERVAL), intent(in) :: x, y
    type(INTERVAL)             :: r

    r%lower = x%lower - y%upper
    r%upper = x%upper - y%lower
end function sub

! mult --
!     Multiply two interval numbers
! Arguments:
!     x        First interval number
!     y        Second interval number
! Result:
!     x*y
!
function mult( x, y ) result(r)
    type(INTERVAL), intent(in) :: x, y
    type(INTERVAL)             :: r

    real(kind=wp)              :: r1, r2, r3, r4

    r1      = x%lower * y%lower
    r2      = x%lower * y%upper
    r3      = x%upper * y%lower
    r4      = x%upper * y%upper
    r%lower = min( r1, r2, r3, r4 )
    r%upper = max( r1, r2, r3, r4 )
end function mult

! div --
!     Divide two interval numbers
! Arguments:
!     x        First interval number
!     y        Second interval number
! Result:
!     x/y
! Note:
!     If the y interval contains zero, this is a
!     division by zero.
!
function div( x, y ) result(r)
    type(INTERVAL), intent(in) :: x, y
    type(INTERVAL)             :: r

    real(kind=wp)              :: r1, r2, r3, r4

    if ( y%lower < 0.0 .and. y%upper > 0.0 ) then
        stop 'Division by zero'
    endif

    r1      = x%lower / y%lower
    r2      = x%lower / y%upper
    r3      = x%upper / y%lower
    r4      = x%upper / y%upper
    r%lower = min( r1, r2, r3, r4 )
    r%upper = max( r1, r2, r3, r4 )
end function div

end module interval_arithmetic
