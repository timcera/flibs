! chk_mismatch_format_data.f90 --
!     Check if the compiler warns about mismatches between the format string and the data
!
program chk_mismatch_format_data
    real, dimension(5) :: x

    write(*,*) 'This program uses a write statement with a mismatch between format and data'

    x =  [0.0, 1.0, 2.0, 3.0, 4.0]

    write(*,'(a,i5)')  'Type mismatch:        ', x(1)

end program chk_mismatch_format_data
