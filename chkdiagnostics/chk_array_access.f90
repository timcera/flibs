! chk_array_access.f90 --
!     Check if the compiler warns about accessing arrays out of bounds (static)
!
program chk_array_access
    integer, dimension(10) :: x

    write(*,*) 'This program accesses an array element outside the bounds'
    write(*,*) 'A compiler should be able to detect this'

    x = 1

    write(*,*) 'Element 11 of 10: ', x(11)

end program chk_array_access
