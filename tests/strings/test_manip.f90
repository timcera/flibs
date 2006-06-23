! test_manip.f90 --
!     Program to test the string manipulation routines
!
!     $Id$
!
program test_manip
    use string_manipulation

    character(len=20) :: string = "Abcdef ghij"
    character(len=50) :: test_line = "the quick brown fox jumped over the lazy dog"

    write(*,*) 'String:   >', string, '<'
    write(*,*) 'Reversed: >', string_reverse(string), '<'

    write(*,*) 'String:    >', test_line, '<'
    write(*,*) 'Uppercase: >', string_toupper(test_line), '<'

    test_line = string_toupper(test_line)
    write(*,*) 'String:    >', test_line, '<'
    write(*,*) 'Uppercase: >', string_tolower(test_line), '<'

    write(*,*) 'Random strings:'
    do i = 1,10
       call random_word( string )
       write(*,*) '    >', string, '<'
    enddo
end program test_manip
