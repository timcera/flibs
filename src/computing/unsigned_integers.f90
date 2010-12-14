! unsigned_integers.f90 --
!    Emulate unsigned integers in Fortran
!
!    Motivation:
!    Fortran does not natively supported unsigned integers. Based on some
!    experiments however, I have concluded that ordinary (signed) integers
!    in Fortran behave the same as unsigned integers except for the
!    following:
!    - Negative values can not be formatted directly
!    - Division requires interpreting the bit patterns in a different way
!
!    Otherwise (under the conditions of 2's completement bit patterns) the
!    results of integer operations are exactly the same - that is: the
!    bit patterns are the same. Most importantly: right shifts in Fortran
!    give the same result as right shifting unsigned integers in C.
!
!    Therefore this module offers several operations for dealing with unsigned
!    integers:
!    - Conversion to and from a string
!    - Division
!    - Comparison
!
!    TODO:
!    Proper test program - compare to C results (using hexadecimal output)
!
module unsigned_integers

    interface operator(.udiv.)
        module procedure unsigned_div
    end interface
    interface operator(.ult.)
        module procedure unsigned_lt
    end interface

    interface to_unsigned
        module procedure to_unsigned_from_string
    end interface

    integer, parameter, private       :: dp = kind(1.0d0)
    real(kind=dp), parameter, private :: offset = 2.0**31
contains

elemental function unsigned_div( a, b )
    integer, intent(in)      :: a
    integer, intent(in)      :: b
    integer                  :: unsigned_div

    real(kind=dp)            :: av
    real(kind=dp)            :: bv
    real(kind=dp)            :: result

    !
    ! Convert the integers
    !
    av = a
    bv = b
    if ( a < 0 ) then
        av = offset + a
    endif
    if ( b < 0 ) then
        bv = offset + b
    endif

    result = av / bv

    if ( result >= offset ) then
        unsigned_div = int( result - offset )
    else
        unsigned_div = int( result )
    endif
end function unsigned_div

elemental function unsigned_lt( a, b )
    integer, intent(in)      :: a
    integer, intent(in)      :: b
    logical                  :: unsigned_lt

    real(kind=dp)            :: av
    real(kind=dp)            :: bv
    real(kind=dp)            :: result

    !
    ! Convert the integers
    !
    av = a
    bv = b
    if ( a < 0 ) then
        av = offset + a
    endif
    if ( b < 0 ) then
        bv = offset + b
    endif

    unsigned_lt = av < bv

end function unsigned_lt

!elemental function to_unsigned_from_string( string )
function to_unsigned_from_string( string )
    character(len=*), intent(in) :: string
    integer                      :: to_unsigned_from_string

    real(kind=dp)                :: result

    !
    ! Do NOT catch any errors - no way to report them properly
    !
    read( string, * ) result
    !
    ! Convert to "unsigned" integer
    !
    if ( result >= offset ) then
        to_unsigned_from_string = offset - result
    else
        to_unsigned_from_string = result
    endif

end function to_unsigned_from_string

end module unsigned_integers

program test_unsigned_integers

    use unsigned_integers

    integer :: a, b

    a = -1
    b = -2

    write( *, * ) 'Result should be 1: ', a .udiv. b
    write( *, * ) 'Result should be 0: ', b .udiv. a
    write( *, * ) 'Result should be true:  ', 1 .ult. a
    write( *, * ) 'Result should be false: ', a .ult. 1
                     !2147483647
    a = to_unsigned( '4000000000' )
    write( *, * ) 'Largest signed integer: ', huge(1), '- result: ', a
    write( *, * ) 'Result should be true:  ', a .ult. huge(1)

end program test_unsigned_integers
