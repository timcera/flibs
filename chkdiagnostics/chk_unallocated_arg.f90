! chk_unallocated_arg.f90 --
!     Check if the compiler warns about the use of an unallocated array/unassociated pointer for an intent(in) argument
!
program chk_unallocated_arg
    implicit none

    integer, dimension(:), allocatable :: x
    integer, dimension(:), pointer     :: y

    write(*,*) 'This program passes an unallocated array and an unassociated pointer to a routine via an intent(in) argument'

    call print_size( x )
    call print_size( y )

contains
subroutine print_size( array )
    integer, dimension(:), intent(in) :: array

    write(*,*) 'Size: ', size(array)
end subroutine print_size

end program chk_unallocated_arg
