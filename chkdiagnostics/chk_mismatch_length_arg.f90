! chk_mismatch_length_arg.f90 --
!     Check if the compiler warns about mismatches in character lengths for actual arguments
!
program chk_mismatch_length_arg
    implicit none

    character(len=20), dimension(2) :: string

    write(*,*) 'This program calls a subroutine and passes character strings of the wrong length'

    string(1) = 'First string'
    string(2) = 'Second entry'

    call print_string( 2, string )

contains

subroutine print_string( n, s )
    integer, intent(in)                        :: n
    character(len=5), dimension(*), intent(in) :: s

    integer                                    :: i

    do i = 1,n
        write(*,*) i, s(i)
    enddo

end subroutine print_string

end program chk_mismatch_length_arg
