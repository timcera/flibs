! reportcmp.f90 --
!     Read the output from the compile/link step and report the
!     number of warnings and error messages
!
program reportcmp
    implicit none

    integer            :: ierr
    integer            :: messages
    character(len=20)  :: type
    character(len=80)  :: name
    character(len=200) :: text

    call get_command_argument( 1, type )
    call get_command_argument( 2, name )

    open( 20, file = 'reportcmp.out', position = 'append' )

    if ( type == 'compiler' ) then
        open( 10, file = '_comp_.out', status = 'old' )
    else
        open( 10, file = '_runexe_.out', status = 'old' )
    endif

    messages = 0

    do
        read( 10, '(a)', iostat = ierr ) text

        if ( ierr /= 0 ) then
            exit
        endif

        if ( index( text, 'Warning:' )          > 0 .or. &
             index( text, 'warning #' )         > 0 .or. &
             index( text, 'remark #' )          > 0 .or. &
             index( text, 'Error:' )            > 0 .or. &
             index( text, 'error #' )           > 0 .or. &
             index( text, 'remark:' )           > 0 .or. &
             index( text, 'Error termination' ) > 0 .or. &
             index( text, 'forrtl:' )   > 0 ) then
            messages = messages + 1
        endif
    enddo

    if ( type == 'compiler' ) then
        write( 20, '(a)' ) ''
        write( 20, '(i5,2(1x,a))' ) messages, 'compiler', trim(name)
    else
        write( 20, '(i5,2(1x,a))' ) messages, 'runtime ', trim(name)
    endif
end program reportcmp
