! chk_real_do_variable.f90 --
!     Check if the compiler warns about real do-variables (deleted feature)
!
program chk_real_do_variable
    implicit none

    real :: x, y

    write(*,*) 'This program uses a DO-loop with a real variable as the DO-variable'

    y = 0.0
    do x = 0.2, 2.0, 0.2
        y = y + x

        write(*,*) x, y
    enddo

end program chk_real_do_variable
