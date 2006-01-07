! select_precision.f90 --
!     Module for defining kinds with single and double precision
!
!     sp: kind for single precision
!     dp: kind for double precision
!
!     wp: kind for working precision (set to either sp or dp)
!
module select_precision
    implicit none

    integer, parameter :: sp = kind( 1.0 )
    integer, parameter :: dp = selected_real_kind( precision( 1.0 ) + 1 )

    integer, parameter :: wp = sp

end module select_precision
