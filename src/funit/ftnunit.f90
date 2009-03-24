! ftnunit.f90 --
!     Module that implements part of the "ftnunit" framework:
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
!     ftnunit_test.f90 -- deprecated
!     runtests.bat
!     runtests.sh
!     runtests.tcl
!
!     TODO:
!     - Get the number of runs right
!     - Get the logic for the first and last tests right
!     - Stylesheet
!
!     $Id$
!
module ftnunit
    implicit none

    integer, private, save :: last_test           ! Last test that was started
    integer, private, save :: testno              ! Current test number
    integer, private, save :: nofails             ! Number of assertions that failed
    integer, private, save :: noruns              ! Number of runs so far
    logical, private, save :: call_final = .true. ! Call runtests_final implicitly?
    logical, private, save :: previous   = .false.! Previous test run?
    integer, private, save :: failed_asserts = 0
    character(len=20), private, save :: html_file = 'ftnunit.html'


    interface assert_equal
        module procedure assert_equal_int
        module procedure assert_equal_int1d
    end interface

    interface assert_comparable
        module procedure assert_comparable_real
        module procedure assert_comparable_real1d
    end interface

contains

! test --
!     Routine to run a unit test
! Arguments:
!     proc          The subroutine implementing the unit test
!     text          Text describing the test
!
subroutine test( proc, text )
    external          :: proc
    character(len=*)  :: text

    integer           :: lun
    integer           :: ierr

    !
    ! Check if the test should run
    !
    testno = testno + 1
    if ( testno <= last_test ) then
        return
    endif

    !
    ! Record the fact that we started the test
    !
    call ftnunit_get_lun( lun )
    open( lun, file = 'ftnunit.lst' )
    write( lun, * ) testno, nofails, noruns, ' ', .true.
    close( lun )

    !
    ! Run the test
    !
    write( *, '(2a)' ) 'Test: ', trim(text)
    call ftnunit_write_html_test_begin( text )

    call proc

    !
    ! No runtime error or premature end of
    ! the program ...
    !
    previous = .true.
    call ftnunit_get_lun( lun )
    open( lun, file = 'ftnunit.lst' )
    write( lun, * ) testno, nofails, noruns, ' ', .true.
    close( lun )

end subroutine test

! runtests_init --
!     Subroutine to initialise the ftnunit system
! Arguments:
!     None
! Note:
!     Use in conjunction with runtests_final to enable multiple calls
!     to the runtests subroutine. This makes it easier to run tests
!     from different modules, as you have more than one subroutine to
!     do the actual tests.
!
subroutine runtests_init
    call_final = .false.

    if ( .not. ftnunit_file_exists("ftnunit.lst") ) then
        call ftnunit_write_html_header
    endif
end subroutine

! runtests_final --
!     Subroutine to report the overall statistics
! Arguments:
!     None
! Note:
!     Use in conjunction with runtests_init to enable multiple calls
!     to the runtests subroutine. This makes it easier to run tests
!     from different modules, as you have more than one subroutine to
!     do the actual tests.
!
subroutine runtests_final
    if ( ftnunit_file_exists("ftnunit.run") ) then
        noruns = max( 1, noruns - 1 )
        write(*,'(a,i5)') 'Number of failed assertions:                ', nofails
        write(*,'(a,i5)') 'Number of runs needed to complete the tests:', noruns
        call ftnunit_remove_file( "ftnunit.lst" )
        call ftnunit_write_html_footer
        stop
    endif
end subroutine

! runtests --
!     Subroutine to run the tests if requested
! Arguments:
!     testproc      The test subroutine that actually runs the unit test
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
    previous  = .false.

    if ( ftnunit_file_exists("ftnunit.run") ) then
        if ( ftnunit_file_exists("ftnunit.lst") ) then
            call ftnunit_get_lun( lun )
            open( lun, file = "ftnunit.lst", iostat = ierr )
            if ( ierr == 0 ) then
                read( lun, *, iostat = ierr ) last_test, nofails, noruns, previous
                if ( ierr /= 0 ) then
                    last_test = 0
                    nofails   = 0
                    noruns    = 0
                    previous  = .false.
                endif
                close( lun )
            endif
            if ( previous ) then
                call ftnunit_write_html_previous_failed
            endif
        endif

        noruns = noruns + 1
        if ( noruns == 1 .and. call_final ) then
            call ftnunit_write_html_header
        endif

        call testproc

        if ( call_final ) then
            call runtests_final
        endif

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
        call ftnunit_write_html_failed_logic( text, .true. )
    endif
end subroutine assert_true

! assert_false --
!     Subroutine to check if a condition is false
! Arguments:
!     cond          Condition to be checked
!     text          Text describing the assertion
! Side effects:
!     If the assertion fails, this is reported to standard
!     output. Also, nofails is increased by one.
!
subroutine assert_false( cond, text )
    logical, intent(in)          :: cond
    character(len=*), intent(in) :: text

    if ( cond ) then
        nofails = nofails + 1
        write(*,*) '    Condition "',trim(text), '" failed'
        write(*,*) '    It should have been false'
        call ftnunit_write_html_failed_logic( text, .false. )
    endif
end subroutine assert_false

! assert_equal_int --
!     Subroutine to check if two integers are equal
! Arguments:
!     value1        First value
!     value2        Second value
!     text          Text describing the assertion
! Side effects:
!     If the assertion fails, this is reported to standard
!     output. Also, nofails is increased by one.
!
subroutine assert_equal_int( value1, value2, text )
    integer, intent(in)          :: value1
    integer, intent(in)          :: value2
    character(len=*), intent(in) :: text

    if ( value1 /= value2) then
        nofails = nofails + 1
        write(*,*) '    Values not equal: "',trim(text), '" - assertion failed'
        write(*,*) '    Values: ', value1, ' and ', value2
        call ftnunit_write_html_failed_int( text, value1, value2 )
    endif
end subroutine assert_equal_int

! assert_equal_int1d --
!     Subroutine to check if two integer arrays are equal
! Arguments:
!     array1        First array
!     array2        Second array
!     text          Text describing the assertion
! Side effects:
!     If the assertion fails, this is reported to standard
!     output. Also, nofails is increased by one.
!
subroutine assert_equal_int1d( array1, array2, text )
    integer, dimension(:), intent(in) :: array1
    integer, dimension(:), intent(in) :: array2
    character(len=*), intent(in)      :: text

    integer                           :: i
    integer                           :: count

    if ( size(array1) /= size(array2) ) then
        nofails = nofails + 1
        write(*,*) '    Arrays have different sizes: "',trim(text), '" - assertion failed'
    else
        if ( any( array1 /= array2 ) ) then
            nofails = nofails + 1
            write(*,*) '    One or more values different: "',trim(text), '" - assertion failed'
            count = 0
            do i = 1,size(array1)
                if ( array1(i) /= array2(i) ) then
                    count = count + 1
                    write(*,'(3a10)')    '    Index', '     First', '    Second'
                    if ( count < 50 ) then
                        write(*,'(3i10)')    i, array1(i), array2(i)
                        call ftnunit_write_html_failed_int1d( &
                            text, i, array1(i), array2(i) )
                    endif
                    write(*,*) 'Number of differences: ', count
                endif
            enddo
        endif
    endif
end subroutine assert_equal_int1d

! assert_comparable_real --
!     Subroutine to check if two reals are approximately equal
! Arguments:
!     value1        First value
!     value2        Second value
!     margin        Allowed margin (relative)
!     text          Text describing the assertion
! Side effects:
!     If the assertion fails, this is reported to standard
!     output. Also, nofails is increased by one.
!
subroutine assert_comparable_real( value1, value2, margin, text )
    real, intent(in)             :: value1
    real, intent(in)             :: value2
    real, intent(in)             :: margin
    character(len=*), intent(in) :: text

    if ( abs(value1-value2) > 0.5 * margin * (abs(value1)+abs(value2)) ) then
        nofails = nofails + 1
        write(*,*) '    Values not comparable: "',trim(text), '" - assertion failed'
        write(*,*) '    Values: ', value1, ' and ', value2
        call ftnunit_write_html_failed_real( text, value1, value2 )
    endif
end subroutine assert_comparable_real

! assert_comparable_real1d --
!     Subroutine to check if two real arrays are comparable
! Arguments:
!     array1        First array
!     array2        Second array
!     text          Text describing the assertion
! Side effects:
!     If the assertion fails, this is reported to standard
!     output. Also, nofails is increased by one.
!
subroutine assert_comparable_real1d( array1, array2, margin, text )
    real, dimension(:), intent(in)    :: array1
    real, dimension(:), intent(in)    :: array2
    real, intent(in)                  :: margin
    character(len=*), intent(in)      :: text

    integer                           :: i
    integer                           :: count

    if ( size(array1) /= size(array2) ) then
        nofails = nofails + 1
        write(*,*) '    Arrays have different sizes: "',trim(text), '" - assertion failed'
    else
        if ( any( abs(array1-array2) > 0.5 * margin * (abs(array1)+abs(array2)) ) ) then
            nofails = nofails + 1
            write(*,*) '    One or more values different: "',trim(text), '" - assertion failed'
            count = 0
            do i = 1,size(array1)
                if ( abs(array1(i)-array2(i)) > &
                         0.5 * margin * (abs(array1(i))+abs(array2(i))) ) then
                    count = count + 1
                    write(*,'(a10,2a15)')    '    Index', '          First', '         Second'
                    if ( count < 50 ) then
                        write(*,'(i10,e15.5)')    i, array1(i), array2(i)
                        call ftnunit_write_html_failed_real1d( &
                            text, i, array1(i), array2(i) )
                    endif
                    write(*,*) 'Number of differences: ', count
                endif
            enddo
        endif
    endif
end subroutine assert_comparable_real1d

! ftnunit_file_exists --
!     Auxiliary function to see if a file exists
! Arguments:
!     filename      Name of the file to check
! Returns:
!     .true. if the file exists, .false. otherwise
!
logical function ftnunit_file_exists( filename )
    character(len=*), intent(in) :: filename

    inquire( file = filename, exist = ftnunit_file_exists )
end function ftnunit_file_exists

! ftnunit_get_lun --
!     Auxiliary subroutine to get a free LU-number
! Arguments:
!     lun           The value that can be used
!
subroutine ftnunit_get_lun( lun )
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

end subroutine ftnunit_get_lun

! ftnunit_remove_file --
!     Auxiliary subroutine to remove a file
! Arguments:
!     filename      Name of the file to be removed
!
subroutine ftnunit_remove_file( filename )
    character(len=*), intent(in) :: filename

    integer                      :: lun
    integer                      :: ierr

    call ftnunit_get_lun( lun )
    open( lun, file = filename, iostat = ierr )
    if ( ierr /= 0 ) then
        write(*,*) '    Could not open file for removal: ', trim(filename)
        nofails = nofails + 1
    else
        close( lun, status = 'delete' )
        if ( ftnunit_file_exists( filename ) ) then
            write(*,*) '    Removal of file unsuccessful: ', trim(filename)
            nofails = nofails + 1
        endif
    endif

end subroutine ftnunit_remove_file

! ftnunit_make_empty_file --
!     Auxiliary subroutine to make an empty file
! Arguments:
!     filename      Name of the file to be created
!
subroutine ftnunit_make_empty_file( filename )
    character(len=*), intent(in) :: filename

    integer                      :: lun
    integer                      :: ierr

    if ( ftnunit_file_exists( filename ) ) then
        call ftnunit_remove_file( filename )
    endif
    call ftnunit_get_lun( lun )
    open( lun, file = filename, iostat = ierr, status = 'new' )
    if ( ierr /= 0 ) then
        write(*,*) '    Failed to create empty file: ', trim(filename)
        nofails = nofails + 1
    else
        close( lun )
    endif

end subroutine ftnunit_make_empty_file

! ftnunit_write_html_header --
!     Auxiliary subroutine to write the header of the HTML file
! Arguments:
!     None
!
subroutine ftnunit_write_html_header
    integer :: lun

    call ftnunit_get_lun( lun )
    open( lun, file = html_file )
    write( lun, '(a)' ) &
        '<html>', &
        '<header>', &
        '<title>Results of unit tests</title>', &
        '</header>', &
        '<body>', &
        '<table border="no">'
    close( lun )

end subroutine ftnunit_write_html_header

! ftnunit_write_html_footer --
!     Auxiliary subroutine to write the footer of the HTML file
! Arguments:
!     None
!
subroutine ftnunit_write_html_footer
    integer :: lun


    call ftnunit_get_lun( lun )
    open( lun, file = html_file, position = 'append' )

    call ftnunit_write_html_close_row( lun )

    write( lun, '(a)' ) &
        '</tr>',    &
        '</table>'
    write( lun,'(a,i5)') '<p>Number of failed assertions: ', nofails
    write( lun,'(a,i5)') '<br>Number of runs needed to complete the tests:', noruns
    write( lun, '(a)' ) &
        '</body>',  &
        '</html>'
    close( lun )

end subroutine ftnunit_write_html_footer

! ftnunit_write_html_test_begin --
!     Auxiliary subroutine to write the test text to the HTML file
! Arguments:
!     text         Description of the test
!
subroutine ftnunit_write_html_test_begin( text )
    character(len=*)  :: text

    integer           :: lun

    call ftnunit_get_lun( lun )
    open( lun, file = html_file, position = 'append' )

    write(*,*) 'test begin: ', previous
    if ( previous ) then
        if ( failed_asserts == 0 ) then
            write( lun, '(a)' ) &
                '<td>OK</td></tr>'
        else
            write( lun, '(a)' ) &
                '</tr>'
        endif
    endif

    failed_asserts = 0

    write( lun, '(a)' ) &
        '<tr>', &
        '<td>', trim(text), '</td>'
    close( lun )

    previous = .true.

end subroutine ftnunit_write_html_test_begin

! ftnunit_write_html_previous_failed --
!     Auxiliary subroutine to write the closing of the failed test
!     from the previous run to the HTML file
! Arguments:
!     None
!
! Note:
!     Apparently the test caused a run-time error, so most probably no
!     assertions were checked.
!
subroutine ftnunit_write_html_previous_failed

    integer           :: lun

    call ftnunit_get_lun( lun )
    open( lun, file = html_file, position = 'append' )

    write( lun, '(a)' ) &
        '<td>Crashed</td></tr>', &
        '<tr><td>Run-time failure: check the log file</td></tr>'

    close( lun )

    failed_asserts = 1 ! Implicit assertion failed that the test will complete

end subroutine ftnunit_write_html_previous_failed

! ftnunit_write_html_close_row --
!     Auxiliary subroutine to write the closing of a row to the HTML file
! Arguments:
!     lun          LU-number for the HTML file
!
subroutine ftnunit_write_html_close_row( lun )
    integer           :: lun

    if ( failed_asserts > 0 ) then
        if ( failed_asserts == 1 ) then
            write( lun, '(a)' ) &
                '<td>Failed</td>'
        endif
        write( lun, '(a)' ) &
            '</tr><tr><td></td>'
    endif
end subroutine ftnunit_write_html_close_row

! ftnunit_write_html_failed_logic --
!     Auxiliary subroutine to write a failed logic assertion to the HTML file
! Arguments:
!     text         Description of the test
!     expected     Expected value
!
subroutine ftnunit_write_html_failed_logic( text, expected )
    character(len=*)  :: text
    logical           :: expected

    integer           :: lun

    call ftnunit_get_lun( lun )
    open( lun, file = html_file, position = 'append' )

    call ftnunit_write_html_close_row( lun )

    write( lun, '(a)' ) &
        '<td>', trim(text), '</td>', &
        '<td>Value should have been "', trim(merge("true ", "false", expected)), '"</td>'
    close( lun )

    failed_asserts = failed_asserts + 1

end subroutine ftnunit_write_html_failed_logic

! ftnunit_write_html_failed_int --
!     Auxiliary subroutine to write a failed integer assertion to the HTML file
! Arguments:
!     text         Description of the test
!     value1       First value
!     value2       Second value
!
subroutine ftnunit_write_html_failed_int( text, value1, value2 )
    character(len=*)  :: text
    integer           :: value1
    integer           :: value2

    integer           :: lun

    call ftnunit_get_lun( lun )
    open( lun, file = html_file, position = 'append' )

    call ftnunit_write_html_close_row( lun )

    write( lun, '(a)' ) &
        '<td>', trim(text), '</td>', &
        '<td>Values: '
    write( lun, '(a,i0,a,i0,a)' ) &
        value1, ' -- ', value2, '</td>'
    close( lun )

    failed_asserts = failed_asserts + 1

end subroutine ftnunit_write_html_failed_int

! ftnunit_write_html_failed_int1d --
!     Auxiliary subroutine to write a failed integer assertion to the HTML file
! Arguments:
!     text         Description of the test
!     idx          Index
!     value1       First value
!     value2       Second value
!
subroutine ftnunit_write_html_failed_int1d( text, idx, value1, value2 )
    character(len=*)  :: text
    integer           :: idx
    integer           :: value1
    integer           :: value2

    integer           :: lun

    call ftnunit_get_lun( lun )
    open( lun, file = html_file, position = 'append' )

    call ftnunit_write_html_close_row( lun )

    write( lun, '(a)' ) &
        '<td>', trim(text), '</td>', &
        '<td>Values at index: '
    write( lun, '(i0,a,i0,a,i0,a)' ) &
        idx, ':', &
        value1, ' -- ', value2, '</td>'
    close( lun )

    failed_asserts = failed_asserts + 1

end subroutine ftnunit_write_html_failed_int1d

! ftnunit_write_html_failed_real --
!     Auxiliary subroutine to write a failed real assertion to the HTML file
! Arguments:
!     text         Description of the test
!     value1       First value
!     value2       Second value
!
subroutine ftnunit_write_html_failed_real( text, value1, value2 )
    character(len=*)  :: text
    real              :: value1
    real              :: value2

    integer           :: lun

    call ftnunit_get_lun( lun )
    open( lun, file = html_file, position = 'append' )

    call ftnunit_write_html_close_row( lun )

    write( lun, '(a)' ) &
        '<td>', trim(text), '</td>', &
        '<td>Values: '
    write( lun, '(a,e15.7,a,e15.7,a)' ) &
        value1, ' -- ', value2, '</td>'
    close( lun )

    failed_asserts = failed_asserts + 1

end subroutine ftnunit_write_html_failed_real

! ftnunit_write_html_failed_real1d --
!     Auxiliary subroutine to write a failed integer assertion to the HTML file
! Arguments:
!     text         Description of the test
!     idx          Index
!     value1       First value
!     value2       Second value
!
subroutine ftnunit_write_html_failed_real1d( text, idx, value1, value2 )
    character(len=*)  :: text
    integer           :: idx
    real              :: value1
    real              :: value2

    integer           :: lun

    call ftnunit_get_lun( lun )
    open( lun, file = html_file, position = 'append' )

    call ftnunit_write_html_close_row( lun )

    write( lun, '(a)' ) &
        '<td>', trim(text), '</td>', &
        '<td>Values at index: '
    write( lun, '(i0,a,e15.7,a,e15.7,a)' ) &
        idx, ':', &
        value1, ' -- ', value2, '</td>'
    close( lun )

    failed_asserts = failed_asserts + 1

end subroutine ftnunit_write_html_failed_real1d

end module ftnunit
