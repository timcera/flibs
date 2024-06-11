! chk_implicit_unseen.f90 --
!     Check if the compiler warns about implicit interfaces - routine NOT visible
!
program chk_implicit_unseen

    write(*,*) 'This program calls a subroutine that has an implicit interface'
    write(*,*) 'The subroutine is not seen, so the program cannot be built'

    call sub( 1.0 )

end program chk_implicit_unseen
