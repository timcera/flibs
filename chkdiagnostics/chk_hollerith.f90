! chk_hollerith.f90 --
!     Check if the compiler warns about holleriths
!
program chk_hollerith

    write(*,*) 'This program uses hollerith constants'

    write(*,100)

100 format(6hHello )

end program chk_hollerith
