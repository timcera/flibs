! test_string_operations.f90 --
!     Test program for various string operations
!
program test_string_operations
    use string_operations

    character(len=:), allocatable :: result
    logical                       :: eof

    write(*,*) 'Replace a substring:'
    result = replace( 'Replace StRings', 'R', 'XXXX' )
    write(*,*) '    >', result, '<'
    result = replace( 'Replace StRings', 'R', 'XXXX', first_only )
    write(*,*) '    >', result, '<'
    result = replace( 'Replace StRings', 'R', 'XXXX', last_only )
    write(*,*) '    >', result, '<'
    result = replace( 'RYeplace StRings', 'RY', 'XXXX', last_only )
    write(*,*) '    >', result, '<'

    ! Check that replacement occurs with the right priority - RY occurs first, so it
    ! should be replaced by 'XXXX', not by 'YY'
    write(*,*) 'Replace two substrings:'
    result = replace( 'RYeplace StRings', [pair('RY', 'XXXX'), pair('R', 'YY')] )
    write(*,*) '   >', result, '<'

    ! Careful here: empty substring!
    result = replace( 'RYeplace StRings', [pair('', 'XXXX'), pair('R', 'YY')] )
    write(*,*) '   >', result, '<'

    ! Nothing to replace
    result = replace( 'RYeplace StRings', [pair('X', 'XXXX'), pair('Z', 'YY')] )
    write(*,*) '   >', result, '<'

    write(*,*) 'Insert a string:'
    result = insert( 'Insert string', prepend, 'X' )
    write(*,*) '   >', result, '<'
    result = insert( 'Insert string', append, 'X' )
    write(*,*) '   >', result, '<'
    result = insert( 'Insert string', 3, 'X' ) ! After "s"
    write(*,*) '   >', result, '<'

    write(*,*) 'Delete a substring:'
    result = delete( 'Delete string', 1, 4 )
    write(*,*) '   >', result, '<'

    result = delete( 'Delete string', 4, 4 )
    write(*,*) '   >', result, '<'

    result = delete( 'Delete string', 20, 4 )
    write(*,*) result

    ! Toupper/tolower
    write(*,*) 'Change case:'
    result = toupper( 'A simple sentence starts with a capital' )
    write(*,*) '   >', result, '<'

    result = tolower( 'A SIMPLE SENTENCE starts with a Capital' )
    write(*,*) '   >', result, '<'

    ! Extensions to trim()
    write(*,*) 'Extensions to trim():'
    result = trimx( 'A SIMPLE SENTENCE AAA AAA ', ' A' )
    write(*,*) '   >', result, '<'
    result = trimxleft( 'A SIMPLE SENTENCE AAA AAA ', ' A' )
    write(*,*) '   >', result, '<'
    result = trimxright( 'A SIMPLE SENTENCE AAA AAA ', ' A' )
    write(*,*) '   >', result, '<'

    !
    ! Read a file line by line
    !
    open( 10, file = 'test_string_operations.inp' )

    do
        call read_line_from_file( 10, result, eof )
        if ( eof ) then
            exit
        endif

        write( *, '(3a)' ) '>', result, '<'
    enddo

end program test_string_operations
