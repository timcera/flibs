! chk_incorrect_format.f90 --
!     Check if the compiler warns about incorrect format strings
!
program chk_incorrect_format
    real, dimension(5) :: x

    write(*,*) 'This program uses several incorrect formats'

    x =  [0.0, 1.0, 2.0, 3.0, 4.0]

    write(*,'(a,(i5)') 'Parenthesis incorrect: ', x(1)
    write(*,'(a,j5)')  'Write edit descriptor: ', 1
    write(*,'(a,5f5)') 'Malformed format:', x

end program chk_incorrect_format
