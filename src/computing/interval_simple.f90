! interval_simple.f90 --
!     Straightforward implementation of interval arithmetic:
!     no dependency analysis
!
!     TODO:
!     Make sure rounding has the right effect!
!
!     TODO:
!     Degenerate intervals, arising from division by zero
!
!     TODO:
!     Make find_root more robust!
!
!     $Id$
!
module interval_arithmetic
    use select_precision
    implicit none

    private
    public   :: interval, find_root
    public   :: intval, mid, operator(+), operator(-), operator(*), operator(/)
    public   :: operator(.pgt.), operator(.cgt.), operator(.pge.), operator(.cge.)
    public   :: operator(.plt.), operator(.clt.), operator(.ple.), operator(.cle.)

    type INTERVAL
        real(kind=wp) :: lower
        real(kind=wp) :: upper
    end type INTERVAL

    interface operator(+)
        module procedure add
        module procedure add_r_i
        module procedure add_i_r
    end interface

    interface operator(-)
        module procedure sub
        module procedure sub_r_i
        module procedure sub_i_r
    end interface

    interface operator(*)
        module procedure mult
        module procedure mult_r_i
        module procedure mult_i_r
    end interface

    interface operator(/)
        module procedure div
        module procedure div_r_i
        module procedure div_i_r
    end interface

    !
    ! Relational operations:
    ! - probably and certainly greater than etc.
    !
    interface operator(.pgt.)
        module procedure pgt
    end interface
    interface operator(.cgt.)
        module procedure cgt
    end interface
    interface operator(.pge.)
        module procedure pge
    end interface
    interface operator(.cge.)
        module procedure cge
    end interface
    interface operator(.plt.)
        module procedure plt
    end interface
    interface operator(.clt.)
        module procedure clt
    end interface
    interface operator(.ple.)
        module procedure ple
    end interface
    interface operator(.cle.)
        module procedure cle
    end interface


contains

! intval --
!     Create an interval number
! Arguments:
!     xmin     Lower bound of the interval
!     xmax     Upper bound of the interval
! Result:
!     New interval number
!
function intval( xmin, xmax ) result(r)
    real(kind=wp), intent(in) :: xmin, xmax
    type(INTERVAL)            :: r

    r%lower = min( xmin, xmax )
    r%upper = max( xmin, xmax )
end function intval

! mid --
!     Return the midpoint of the interval
! Arguments:
!     x       Interval number
! Result:
!     Midpoint of the interval
!
function mid( x ) result(r)
    type(INTERVAL)            :: x
    real(kind=wp)             :: r

    r = 0.5 * (x%lower + x%upper)
end function mid

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

function add_r_i( x, y ) result(r)
    real(kind=wp), intent(in)  :: x
    type(INTERVAL), intent(in) :: y
    type(INTERVAL)             :: r

    r%lower = x + y%lower
    r%upper = x + y%upper
end function add_r_i

function add_i_r( x, y ) result(r)
    type(INTERVAL), intent(in) :: x
    real(kind=wp), intent(in)  :: y
    type(INTERVAL)             :: r

    r%lower = x%lower  + y
    r%upper = x%upper  + y
end function add_i_r

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

function sub_r_i( x, y ) result(r)
    real(kind=wp), intent(in)  :: x
    type(INTERVAL), intent(in) :: y
    type(INTERVAL)             :: r

    r%lower = x - y%upper
    r%upper = x - y%lower
end function sub_r_i

function sub_i_r( x, y ) result(r)
    type(INTERVAL), intent(in) :: x
    real(kind=wp), intent(in)  :: y
    type(INTERVAL)             :: r

    r%lower = x%lower - y
    r%upper = x%upper - y
end function sub_i_r

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

function mult_r_i( x, y ) result(r)
    real(kind=wp), intent(in)  :: x
    type(INTERVAL), intent(in) :: y
    type(INTERVAL)             :: r

    r%lower = x * y%lower
    r%upper = x * y%upper
end function mult_r_i

function mult_i_r( x, y ) result(r)
    type(INTERVAL), intent(in) :: x
    real(kind=wp), intent(in)  :: y
    type(INTERVAL)             :: r

    r%lower = x%lower  * y
    r%upper = x%upper  * y
end function mult_i_r

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

function div_r_i( x, y ) result(r)
    real(kind=wp), intent(in)  :: x
    type(INTERVAL), intent(in) :: y
    type(INTERVAL)             :: r, xi

    xi = intval(x,x)
    r = div( xi, y )
end function div_r_i

function div_i_r( x, y ) result(r)
    type(INTERVAL), intent(in) :: x
    real(kind=wp), intent(in)  :: y
    type(INTERVAL)             :: r

    r%lower = x%lower / y
    r%upper = x%upper / y
end function div_i_r

! pgt and others --
!     Determine relation between two interval numbers
! Arguments:
!     x        First interval number
!     y        Second interval number
! Result:
!     x/y
! Note:
!     Probably greater: the interval of x extends further to the
!     positive side than that of y
!     Certainly greater: the complete interval of x lies right from that of y
!     Other relations are similar
!
logical function pgt( x, y )
    type(INTERVAL), intent(in) :: x, y

    pgt = x%upper > y%upper
end function pgt

logical function cgt( x, y )
    type(INTERVAL), intent(in) :: x, y

    cgt = x%lower > y%upper
end function cgt

logical function pge( x, y )
    type(INTERVAL), intent(in) :: x, y

    pge = x%upper >= y%upper
end function pge

logical function cge( x, y )
    type(INTERVAL), intent(in) :: x, y

    cge = x%lower >= y%upper
end function cge

logical function plt( x, y )
    type(INTERVAL), intent(in) :: x, y

    plt = x%lower < y%lower
end function plt

logical function clt( x, y )
    type(INTERVAL), intent(in) :: x, y

    clt = x%upper < y%lower
end function clt

logical function ple( x, y )
    type(INTERVAL), intent(in) :: x, y

    ple = x%lower <= y%lower
end function ple

logical function cle( x, y )
    type(INTERVAL), intent(in) :: x, y

    cle = x%upper <= y%lower
end function cle

! find_root
!     Find a root using Newton-Raphson
! Arguments:
!     f          Function whose root we want to find
!     fprime     First derivative of this function
!     xinit      Initial estimate (interval valued)
!     tolerance  Tolerance in finding root
!     root       Final estimate (interval valued)
!     found      Whether it was found or not
! Note:
!     If the iteration does not converge, we assume
!     that the iterate will grow indefinitely.
!
!     If you need a more general interface to the function,
!     consider using the implementation as a template
!
subroutine find_root( f, fprime, xinit, tolerance, root, found )
    interface
        function f(x)
            use select_precision
            real(kind=wp), intent(in) :: x
            real(kind=wp)             :: f
        end function
    end interface
    interface
        function fprime(x)
            use select_precision
            real(kind=wp), intent(in) :: x
            real(kind=wp)             :: fprime
        end function
    end interface

    type(INTERVAL), intent(in)  :: xinit
    type(INTERVAL), intent(out) :: root
    real(kind=wp)               :: tolerance
    logical                     :: found

    integer                     :: iter
    integer, parameter          :: maxiter = 1000
    real(kind=wp)               :: fvalue   ! Real valued!
    type(INTERVAL)              :: fpvalue
    real(kind=wp)               :: fleft
    real(kind=wp)               :: fright
    real(kind=wp)               :: fpleft
    real(kind=wp)               :: fpright

    found = .false.
    root  = xinit

    do iter = 1,maxiter

        fpleft  = fprime(root%lower)
        fpright = fprime(root%upper)
        fvalue  = f(mid(root))
        fpvalue = intval( fpleft, fpright )

        call find_root_iter( fvalue, fpvalue, root, tolerance, found )
        if ( found .or. abs(mid(root)) > huge(1.0_wp)/10.0 ) exit
    enddo

end subroutine find_root

! find_root_iter
!     Do one iteration in the Newton-Raphson method
! Arguments:
!     fvalue     Function value - real valued!
!     fpvalue    Function derivative - interval valued!
!     root       Current iterate
!     tolerance  Tolerance in finding root
!     found      Whether it was found or not
!
subroutine find_root_iter( fvalue, fpvalue, root, tolerance, found )
    real(wp), intent(in)           :: fvalue
    type(INTERVAL), intent(in)     :: fpvalue
    type(INTERVAL), intent(inout)  :: root
    real(wp), intent(in)           :: tolerance
    logical, intent(out)           :: found

    type(INTERVAL)                 :: newroot

    newroot = root - fvalue / fpvalue
    found   = abs( mid(root) - mid(root) ) < tolerance
    root    = newroot

end subroutine find_root_iter

end module interval_arithmetic
