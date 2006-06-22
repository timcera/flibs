! stringmanip.f90 --
!     Module for simple string manipulations:
!
!     string_reverse         Return the reverted string
!     string_tolower         Return the string with upper-case letters translated to lower-case
!     string_toupper         Return the string with lower-case letters translated to upper-case
!
!     The functions actually perform fairly simple string manipulations.
!     It is just that these manipulations occur frequently.
!
!     $Id$
!
module string_manipulation
    implicit none

    character(len=26), parameter, private :: upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=26), parameter, private :: lower = 'abcdefghijklmnopqrstuvwxyz'
contains

! string_reverse --
!     Return a string that has all characters in reverse order
! Arguments:
!     string     String to be reversed
! Result:
!     Reversed string
!
function string_reverse( string ) result (rev)
    character(len=*)           :: string

    character(len=len(string)) :: rev

    integer                    :: i
    integer                    :: length

    length = len(string)
    do i = 1,length
        rev(i:i) = string(length-i+1:length-i+1)
    enddo
end function string_reverse

! string_toupper --
!     Return a string that has all _letters_ in upper case
! Arguments:
!     string     String to be treated
! Result:
!     String with letters turned into upper case
!
function string_toupper( string ) result (new)
    character(len=*)           :: string

    character(len=len(string)) :: new

    integer                    :: i
    integer                    :: k
    integer                    :: length

    length = len(string)
    new    = string
    do i = 1,length
        k = index( string(i:i), lower )
        if ( k > 0 ) then
           new(i:i) = upper(k:k)
        endif
    enddo
end function string_upper

! string_tolower --
!     Return a string that has all _letters_ in lower case
! Arguments:
!     string     String to be treated
! Result:
!     String with letters turned into lower case
!
function string_tolower( string ) result (new)
    character(len=*)           :: string

    character(len=len(string)) :: new

    integer                    :: i
    integer                    :: k
    integer                    :: length

    length = len(string)
    new    = string
    do i = 1,length
        k = index( string(i:i), upper )
        if ( k > 0 ) then
           new(i:i) = lower(k:k)
        endif
    enddo
end function string_tolower

! random_word --
!     Fill a string with a random sequence of letters
! Arguments:
!     string     String to be filled
! Result:
!     String with random letters
!
subroutine random_word( string )
    character(len=*)           :: string

    integer                    :: i
    integer                    :: k
    integer                    :: length
    real                       :: r

    string = ' '
    length = len(string)
    call random_number( r )
    length = 2 + (length-2) * r
    do i = 1,length
        call random_number( r )
        k = 1 + 26 * r
        if ( k > 26 ) k = 1
        string(i:i) = lower(k:k)
    enddo
end subroutine random_word

end module string_manipulation
