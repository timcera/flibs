! funit_test.f90 --
!     Include file containing the source code for the
!     general "test" subroutine
!
!     $Id$
!
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
    call funit_get_lun( lun )
    open( lun, file = 'funit.lst' )
    write( lun, * ) testno, nofails, noruns
    close( lun )

    !
    ! Run the test
    !
    write( *, '(2a)' ) 'Test: ', trim(text)

    call proc

    !
    ! No runtime error or premature end of
    ! the program ...
    !
    call funit_get_lun( lun )
    open( lun, file = 'funit.lst' )
    write( lun, * ) testno, nofails, noruns
    close( lun )

end subroutine test
