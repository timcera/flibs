! chk_temporary_array.f90 --
!     Check if the compiler warns about implicit generation of temporary arrays
!
program chk_temporary_array
    real, dimension(:), allocatable :: x

    write(*,*) 'This program passes an array section to a subroutine in FORTRAN 77 style'
    write(*,*) 'This will most likely cause a temporary array to be created'

    allocate( x(1000000) )
    call random_number( x )

    call sub( x(1::1000), size(x(1::1000)) )

contains

subroutine sub( y, n )
    real    :: y(*)
    integer :: n

    integer :: i
    real    :: sum

    sum = 0.0
    do i = 1,n
        sum = sum + y(i)
    enddo
    write(*,*) 'Sum: ', sum
end subroutine sub

end program chk_temporary_array
