! chk_implicit_interface.f90 --
!     Check if the compiler warns about implicit interfaces - routine visible
!
program chk_implicit_interface

    write(*,*) 'This program calls a subroutine that has an implicit interface'
    write(*,*) 'The argument list is correct'

    call sub( 1.0 )

end program chk_implicit_interface

subroutine sub( x )
    real :: x

    write(*,*) x
end subroutine sub
