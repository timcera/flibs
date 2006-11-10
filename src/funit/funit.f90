! funit.f90 --
!     Module that implements part of the "funit" framework:
!     It is inspired by the well-known JUnit framework for
!     integrating unit tests in a Java application.
!
!     The module offers:
!     - a set of common utilities, such as assertion checking
!       routines
!     - a general routine to run the tests if requested
!     - resources that keep track of the status
!
!     Related files:
!     funit_test.f90
!     runtests.bat
!     runtests.sh
!     runtests.tcl
!
!     $Id$
!
module funit
    implicit none

    integer :: last_test         ! Last test that was started
    integer :: testno            ! Current test number
    integer :: nofails           ! Number of assertions that failed
    integer :: noruns            ! Number of runs so far

contains

! runtests --
!     Subroutine to run the tests if requested
! Arguments:
!     testproc      The test subroutine that actually runs the unit test
! Side effects:
!     The input file with data is opened at LUN 10,
!     the report file is opened at LUN 20
!
subroutine runtests( testproc )
    interface
        subroutine testproc
        end subroutine testproc
    end interface

    integer :: lun
    integer :: ierr

    last_test = 0
    nofails   = 0
    noruns    = 0
    testno    = 0

    if ( funit_file_exists("funit.run") ) then
        if ( funit_file_exists("funit.lst") ) then
            call funit_get_lun( lun )
            open( lun, file = "funit.lst", iostat = ierr )
            if ( ierr == 0 ) then
                read( lun, *, iostat = ierr ) last_test, nofails, noruns
                if ( ierr /= 0 ) then
                    last_test = 0
                    nofails   = 0
                    noruns    = 0
                endif
                close( lun )
            endif
        endif

        noruns = noruns + 1

        call testproc

        write(*,'(a,i5)') 'Number of failed assertions:                ', nofails
        write(*,'(a,i5)') 'Number of runs needed to complete the tests:', noruns
        call funit_remove_file( "funit.lst" )
        stop
    endif

end subroutine runtests

! assert_true --
!     Subroutine to check if a condition is true
! Arguments:
!     cond          Condition to be checked
!     text          Text describing the assertion
! Side effects:
!     If the assertion fails, this is reported to standard
!     output. Also, nofails is increased by one.
!
subroutine assert_true( cond, text )
    logical, intent(in)          :: cond
    character(len=*), intent(in) :: text

    if ( .not. cond ) then
        nofails = nofails + 1
        write(*,*) '    Condition "',trim(text), '" failed'
        write(*,*) '    It should have been true'
    endif
end subroutine assert_true

! funit_file_exists --
!     Auxiliary function to see if a file exists
! Arguments:
!     filename      Name of the file to check
! Returns:
!     .true. if the file exists, .false. otherwise
!
logical function funit_file_exists( filename )
    character(len=*), intent(in) :: filename

    inquire( file = filename, exist = funit_file_exists )
end function funit_file_exists

! funit_get_lun --
!     Auxiliary subroutine to get a free LU-number
! Arguments:
!     lun           The value that can be used
!
subroutine funit_get_lun( lun )
    integer, intent(out) :: lun

    logical       :: opend
    integer, save :: prevlun = 0

    if ( prevlun /= 0 ) then
        inquire( unit = lun, opened = opend )
        if ( .not. opend ) then
            lun = prevlun
            return
        endif
    endif

    do prevlun = 10,99
        inquire( unit = prevlun, opened = opend )
        if ( .not. opend ) then
            lun = prevlun
            return
        endif
    enddo

end subroutine funit_get_lun

! funit_remove_file --
!     Auxiliary subroutine to remove a file
! Arguments:
!     filename      Name of the file to be removed
!
subroutine funit_remove_file( filename )
    character(len=*), intent(in) :: filename

    integer                      :: lun
    integer                      :: ierr

    call funit_get_lun( lun )
    open( lun, file = filename, iostat = ierr )
    if ( ierr /= 0 ) then
        write(*,*) '    Could not open file for removal: ', trim(filename)
        nofails = nofails + 1
    else
        close( lun, status = 'delete' )
        if ( funit_file_exists( filename ) ) then
            write(*,*) '    Removal of file unsuccssful: ', trim(filename)
            nofails = nofails + 1
        endif
    endif

end subroutine funit_remove_file

! funit_make_empty_file --
!     Auxiliary subroutine to make an empty file
! Arguments:
!     filename      Name of the file to be created
!
subroutine funit_make_empty_file( filename )
    character(len=*), intent(in) :: filename

    integer                      :: lun
    integer                      :: ierr

    if ( funit_file_exists( filename ) ) then
        call funit_remove_file( filename )
    endif
    call funit_get_lun( lun )
    open( lun, file = filename, iostat = ierr, status = 'new' )
    if ( ierr /= 0 ) then
        write(*,*) '    Failed to create empty file: ', trim(filename)
        nofails = nofails + 1
    else
        close( lun )
    endif

end subroutine funit_make_empty_file

end module funit
