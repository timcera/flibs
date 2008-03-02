! fwrapper.f90 --
!     Program to generate a C wrapper libeary from Fortran source
!     code, such that the Fortran routines can be used from C.
!
!     TODO:
!     Almost everything
!     Watch out for moduleprocedure
!
!     $Id$
!
program fwrapper
    implicit none
    character(len=20) :: srcname
    character(len=20) :: filename
    integer           :: lufiles    = 10
    integer           :: lusrc      = 11
    integer           :: luchead    = 12
    integer           :: lucwrap    = 13
    integer           :: lufwrap    = 14
    integer           :: lufstrip   = 15
    integer           :: ierr
    integer           :: k
    logical           :: first      = .true.

    open( lufiles, file = 'fwrapper.inp', status = 'old' )

    do
        read( lufiles, '(a)', iostat = ierr ) srcname
        if ( ierr /= 0 ) exit

        !
        ! First step: open the source file, and if not done yet,
        ! the files with the resulting C and Fortran wrapper code
        !
        open( lusrc, file = srcname, status = 'old' )

        k       = index( srcname, '.', .true. )
        srcname = srcname(1:k-1)

        if ( first ) then
            first = .false.
            filename = trim(srcname) // "_wrap.h"
            open( luchead, file = filename )
            filename = trim(srcname) // "_wrap.c"
            open( lucwrap, file = filename )
            filename = trim(srcname) // "_wrap.f90"
            open( lufwrap, file = filename )
        endif

        !
        ! Second step: strip the source code
        !
        open( lufstrip, file = 'fwrapper.stripped' )
        call strip_source( lusrc, lufstrip )
        close( lusrc )
        rewind( lufstrip )

    enddo

contains

! lower_case --
!     Return the lower case version of a character
!
! Arguments:
!     ch              Character to be converted
!
character(len=1) function lower_case( ch )
    character(len=1), intent(in) :: ch

    integer, parameter :: shift = iachar('a') - iachar('A')

    if ( iachar(ch) >= iachar('A') .and. iachar(ch) <= iachar('Z') ) then
        lower_case = achar(iachar(ch)+shift)
    else
        lower_case = ch
    endif
end function lower_case

! strip_source --
!     Strip the source code from all uninteresting bits
!
! Arguments:
!     lusrc           LU-number of the source code
!     lufstrip        LU-number of the temporary file
!
! Note:
!     For the moment only free-form source files!
!
subroutine strip_source( lusrc, lufstrip )
    integer, intent(in) :: lusrc
    integer, intent(in) :: lufstrip

    character(len=150)  :: line         ! More than enough, even for wide source files
    character(len=150)  :: compact_line
    integer             :: ierr
    integer             :: i
    integer             :: j
    integer             :: k
    integer             :: length
    logical             :: continued

    character(len=20), dimension(18) :: keyword = &
        (/ 'type', 'result', 'subroutine', 'function', 'recursive', 'module', &
           'integer', 'real', 'doubleprecision', 'logical', 'character',      &
           'complex', 'endfunction', 'endsubroutine', 'endtype', 'endmodule', &
           'parameter', 'dimension'                                           /)

    continued = .false.
    do
        read( lusrc, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) exit

        !
        ! Remove any comments and empty lines
        !
        k = index( line, '!' )
        if ( k > 0 ) then
            line = line(1:k-1)
        endif
        if ( line == ' ' ) cycle

        !
        ! Remove all spaces - they are of little interest
        ! NOTE: exception: in parameter statements
        !
        length = len_trim(line)
        compact_line = ' '
        j      = 0
        do i =1,length
            if ( line(i:i) /= ' ' ) then
                j = j + 1
                compact_line(j:j) = lower_case(line(i:i))
            endif
        enddo

        !
        ! Now: filter out those lines that are not part of a declaration
        !

        if ( compact_line == 'end' .or. continued ) then
            continued = index( compact_line, '&' ) > 0
            write( lufstrip, '(a)' ) compact_line
            cycle
        endif
        do j = 1,size(keyword)
            if ( index( compact_line, trim(keyword(j)) ) == 1 ) then
                continued = index( compact_line, '&' ) > 0
                write( lufstrip, '(a)' ) compact_line
                exit
            endif
        enddo
    enddo

end subroutine strip_source

end program
