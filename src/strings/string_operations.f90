! string_operations.f90 --
!     Module for replacing substrings and other operations on strings
!
!     Note:
!     Two versions of the basic routine are supplied:
!     - Replace one particular substring by another string
!     - Replace a set of substrings by corresponding strings
!
!     In principle all substrings are replaced, but you can
!     control this in the first version via an optional
!     argument, taking the values: replace_all, first_only and last_only
!
!     The derived type pair can be used to specify which substring
!     must be replaced by what other string:
!
!     newstring = replace( string, [pair('A', 'a'), pair('B', 'b'), ...])
!
module string_operations
    implicit none

    type pair
        character(len=:), allocatable :: substring
        character(len=:), allocatable :: replacement
    end type

    interface replace
         module procedure replace_single
         module procedure replace_pairs
    end interface

    integer, parameter :: replace_all = 1
    integer, parameter :: first_only  = 2
    integer, parameter :: last_only   = 3
    integer, parameter :: prepend     = 0
    integer, parameter :: append      = -1

contains

! replace_single --
!     Replace one or all occurrences of a single substring
!
! Arguments:
!     input          The input string
!     substring      The substring to be replaced
!     replacement    The replacing string
!     mode           Optional argument determining how many substrings
!                    are to be replaced
!
function replace_single( input, substring, replacement, mode )
    character(len=*), intent(in)  :: input
    character(len=*), intent(in)  :: substring
    character(len=*), intent(in)  :: replacement
    integer, intent(in), optional :: mode

    character(len=:), allocatable :: replace_single

    integer                       :: k, pos, mode_
    logical                       :: back

    ! Careful with zero-length substrings
    if ( substring == '' ) then
        replace_single = input
        return
    endif

    mode_ = replace_all
    if ( present(mode) ) then
        mode_ = mode
    endif

    back = ( mode_ == last_only )

    !
    ! Find the substring
    !
    pos            = 1
    replace_single = ''

    do
         k = index( input(pos:), substring, back )
         !write(*,*) 'k = ', k

         if ( k >= 1 ) then
             replace_single = replace_single // input(pos:pos+k-2) // replacement
             pos = pos + k + len(substring) - 1
         else
             exit
         endif

         if ( mode_ /= replace_all ) then
             exit
         endif
    enddo

    replace_single = replace_single // input(pos:)

end function replace_single

! replace_pairs --
!     Replace all occurrences of substrings by replacements
!
! Arguments:
!     input          The input string
!     pairs          Pairs of substrings and their replacements
!
function replace_pairs( input, pairs )
    character(len=*), intent(in)         :: input
    type(pair), dimension(:), intent(in) :: pairs

    character(len=:), allocatable        :: replace_pairs

    integer                              :: i, k, kfirst, p, pos

    !
    ! Find the substrings one by one
    !
    pos           = 1
    replace_pairs = ''

    do
        kfirst = len(input) + 1
        p      = 0
        do i = 1,size(pairs)
            if ( pairs(i)%substring == '' ) then
                cycle
            endif

            k = index( input(pos:), pairs(i)%substring )

            if ( k > 0 .and. k < kfirst ) then
                kfirst = k
                p      = i
            endif
        enddo

        if ( p >= 1 ) then
            replace_pairs = replace_pairs // input(pos:pos+kfirst-2) // pairs(p)%replacement
            pos = pos + kfirst + len(pairs(p)%substring) - 1
        else
            exit
        endif

        if ( len(replace_pairs) > 30 ) then
            exit
        endif
    enddo

    replace_pairs = replace_pairs // input(pos:)

end function replace_pairs

! insert --
!     Insert a string into another strings at a given position
!
! Arguments:
!     input          The input string
!     pos            Position where the new string is to be placed
!     string         String to be inserted
!
! Note:
!     The inserted string is inserted after the given position
!     If the position is before the first character, the string will be
!     prepended. If the position is after the last character, it will
!     be appended.
!
function insert( input, pos, string )
    character(len=*), intent(in)         :: input
    integer, intent(in)                  :: pos
    character(len=*), intent(in)         :: string

    character(len=:), allocatable        :: insert

    ! Check the position - in this order
    if ( pos >= len(input) .or. pos == append ) then
        insert = input // string
        return
    endif
    if ( pos < 1 .or. pos == prepend ) then
        insert = string // input
        return
    endif

    ! Position within the string
    insert = input(1:pos) // string // input(pos+1:)

end function insert

! delete --
!     Delete a substring from a string
!
! Arguments:
!     input          The input string
!     pos            Position where the substring starts
!     length         Length of the substring
!
! Note:
!     If the given position is zero or negative it is corrected to 1.
!     If the position is beyond the end of the input string, no substring
!     is deleted. The same goes for zero or negative length.
!
function delete( input, pos, length )
    character(len=*), intent(in)         :: input
    integer, intent(in)                  :: pos
    integer, intent(in)                  :: length

    character(len=:), allocatable        :: delete
    integer                              :: startpos, endpos

    ! Check the position
    startpos = pos
    endpos   = min( startpos + length - 1, len(input) )
    if ( pos < 1 ) then
        startpos = 1
        endpos   = min( startpos + length - 1, len(input) )
    endif

    if ( startpos == 1 ) then
        delete = input(endpos+1:)
    else
        delete = input(1:startpos-1) // input(endpos+1:)
    endif

end function delete

! tolower --
!     Convert a string to lower case - assuming ASCII characters only!
!
! Arguments:
!     input          The input string
!
function tolower( input )
    character(len=*), intent(in)         :: input
    character(len=len(input))            :: tolower

    integer                              :: i
    integer                              :: ach
    integer, parameter                   :: ascii_a         = iachar('a')
    integer, parameter                   :: ascii_capital_z = iachar('Z')
    integer, parameter                   :: ascii_capital_a = iachar('A')
    integer, parameter                   :: offset          = ascii_a - ascii_capital_a

    tolower = input

    do i = 1,len(input)
        ach = iachar(tolower(i:i))
        if ( ach >= ascii_capital_a .and. ach <= ascii_capital_z ) then
            tolower(i:i) = achar(ach + offset)
        endif
    enddo

end function tolower

! toupper --
!     Convert a string to upper case - assuming ASCII characters only!
!
! Arguments:
!     input          The input string
!
function toupper( input )
    character(len=*), intent(in)         :: input
    character(len=len(input))            :: toupper

    integer                              :: i
    integer                              :: ach
    integer, parameter                   :: ascii_a         = iachar('a')
    integer, parameter                   :: ascii_z         = iachar('z')
    integer, parameter                   :: ascii_capital_a = iachar('A')
    integer, parameter                   :: offset          = ascii_a - ascii_capital_a

    toupper = input

    do i = 1,len(input)
        ach = iachar(toupper(i:i))
        if ( ach >= ascii_a .and. ach <= ascii_z ) then
            toupper(i:i) = achar(ach - offset)
        endif
    enddo

end function toupper

! trimx --
!     Remove leading and trailing characters from a string
!
! Arguments:
!     input          The input string
!     set            The set of characters that should be removed
!
function trimx( input, set )
    character(len=*), intent(in)         :: input
    character(len=*), intent(in)         :: set
    character(len=:), allocatable        :: trimx

    integer                              :: start, stop

    start = verify( input, set )
    stop  = verify( input, set, .true. )

    trimx = input(start:stop)

end function trimx

! trimxleft --
!     Remove leading characters from a string
!
! Arguments:
!     input          The input string
!     set            The set of characters that should be removed
!
function trimxleft( input, set )
    character(len=*), intent(in)         :: input
    character(len=*), intent(in)         :: set
    character(len=:), allocatable        :: trimxleft

    integer                              :: start

    start = verify( input, set )

    trimxleft = input(start:)

end function trimxleft

! trimxright --
!     Remove leading characters from a string
!
! Arguments:
!     input          The input string
!     set            The set of characters that should be removed
!
function trimxright( input, set )
    character(len=*), intent(in)         :: input
    character(len=*), intent(in)         :: set
    character(len=:), allocatable        :: trimxright

    integer                              :: stop

    stop  = verify( input, set, .true. )

    trimxright = input(:stop)

end function trimxright

! read_line_from_file --
!    Read one complete line as a string from a file
! Arguments:
!    lun         LU-number of the file to be read
!    text        Text string to be created/filled
!    eof         Whether end-of-file was reached or not
! Side effects:
!    Text string is properly initialised
!
subroutine read_line_from_file( lun, text, eof )
    integer, intent(in)              :: lun
    character(len=:), allocatable    :: text
    logical, intent(out)             :: eof

    integer, parameter               :: SEGMENT_LENGTH = 40

    character(len=SEGMENT_LENGTH)    :: segment
    integer                          :: reclen
    logical                          :: eor

    eof  = .false.

    text = ''

    !
    ! Read the record in segments
    !
    do
        read( lun, end = 180, eor = 170, fmt = '(a)', &
              size = reclen, advance='no') segment
        !
        ! Not at the end yet, add to the string
        !
        text = text // segment
    enddo

    !
    ! End of record (end of file will give an empty text string)
    !
170 continue
    text = text // segment(1:reclen)
    return

    !
    ! End of file
    !
180 continue
    text = ''
    eof = .true.
    return

    !
    ! Errors are not handled explicitly ...
    !
end subroutine read_line_from_file

end module string_operations
