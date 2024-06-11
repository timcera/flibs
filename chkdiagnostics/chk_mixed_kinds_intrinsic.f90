! chk_mixed_kinds_intrinsic.f90 --
!     Check if the compiler warns about the use of different kinds in the same call to an intrinsic
!     (Adopted from a question by Al Greynolds on comp.lang.fortran)
!
program chk_mixed_kinds_intrinsic
    implicit none

    integer            :: x
    integer, parameter :: ip = selected_int_kind(1)
    integer(kind=ip)   :: y

    write(*,*) 'This program uses the max function with arguments of diffent kind'
    write(*,*) 'Apparently it is accepted as an extension by many compilers'

    x = 1
    y = 2

    write(*,*) 'Maximum of x and y: ', max( x, y )
    write(*,*) 'Kinds of x and y:   ', kind(x), kind(y)

end program chk_mixed_kinds_intrinsic
