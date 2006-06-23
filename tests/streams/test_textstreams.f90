! Test program for text streams
!
!     $Id$
!
program test_textstream
    use text_streams
    implicit none

    type(TEXT_STREAM)   :: stream
    logical             :: error
    integer             :: ierr
    character(len=20)   :: char

    call textstream_open( stream, 10, 'test_textstream.inp', error )

    if ( error ) then
        write(*,*) 'Problem opening the file!'
        call textstream_close( stream )
        stop 'Error!'
    endif

    do while ( ierr == 0 )
        call textstream_read( stream, char, ierr )
        write(*,*) 'Read: ', char, ' - ', ierr
    enddo

    call textstream_close( stream )
end program test_textstream
