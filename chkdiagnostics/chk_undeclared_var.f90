! chk_undeclared_var.f90 --
!     Check if the compiler warns about undeclared variables and arguments
!
program chk_undeclared_var
    ! This one does NOT use
    ! implicit none
    ! - so old-school convention used ...
    !

    write(*,*) 'This program simply uses variables without declaring them'

    ix = 3

    call print( ix )

contains
subroutine print( iy )

    write(*,*) 'Value: ', iy
end subroutine print

end program chk_undeclared_var
