! pointsets.f90 --
!     Collection of routines to generate sets of points in
!     one, two or three dimensions with certain properties
!
!     $Id$
!
module pointsets
    use select_precision
    implicit none

contains

! arithmetic_spacing --
!     Generate uniformly spaced coordinates
! Arguments:
!     x       Array to be filled
!     begin   First bound of the interval
!     end     Second bound of the interval
! Result:
!     Array x filled with values between begin and end
!
subroutine arithmetic_spacing( x, begin, end )
    real(kind=wp), dimension(:), intent(inout) :: x
    real(kind=wp), intent(in)                  :: begin
    real(kind=wp), intent(in)                  :: end

    integer                                    :: i
    real(kind=wp)                              :: step

    if ( size(x) <= 1 ) return

    step = (end-begin)/(size(x)-1)
    x(1) = begin
    do i = 2,size(x)
        x(i) = x(i-1) + step
    enddo
end subroutine arithmetic_spacing

! geometric_spacing --
!     Generate coordinates spaced as a geometric series
! Arguments:
!     x       Array to be filled
!     begin   First bound of the interval
!     end     Second bound of the interval
!     ratio   Ratio between successive spacings
! Result:
!     Array x filled with values between begin and end
!
subroutine geometric_spacing( x, begin, end, ratio )
    real(kind=wp), dimension(:), intent(inout) :: x
    real(kind=wp), intent(in)                  :: begin
    real(kind=wp), intent(in)                  :: end

    integer                                    :: i
    real(kind=wp)                              :: step
    real(kind=wp)                              :: scale

    if ( size(x) <= 1 ) return
    if ( ratio <= 0.0 ) return
    if ( ratio == 1.0 ) return

    step  = ratio
    x(1)  = 0.0
    do i = 2,size(x)
        x(i) = x(i-1) + step
        step = step * ratio
    enddo

    scale = (end-begin) / (x(size(x))-x(1)
    x = begin + scale * x
end subroutine geometric_spacing

! random_spacing --
!     Generate coordinates randomly spaced over an interval
! Arguments:
!     x       Array to be filled
!     begin   First bound of the interval
!     end     Second bound of the interval
! Result:
!     Array x filled with values between begin and end,
!     all coordinates non-decreasing
!
subroutine random_spacing( x, begin, end, ratio )
    real(kind=wp), dimension(:), intent(inout) :: x
    real(kind=wp), intent(in)                  :: begin
    real(kind=wp), intent(in)                  :: end

    integer                                    :: i
    real(kind=wp)                              :: scale

    if ( size(x) <= 1 ) return

    call random_number( x )
    scale = end-begin
    x(1)  = 0.0
    do i = 2,size(x)
        x(i) = x(i-1) + x(i)
    enddo

    scale = (end-begin) / sum(x)
    x = begin + scale * x
end subroutine random_spacing

! random_2d_block --
!     Generate points within a two-dimensional block
!     that are uniformly distributed
! Arguments:
!     x       Array of x-coordinates to be filled
!     y       Array of y-coordinates to be filled
!     length  Length of the block
!     width   Width of the block
! Result:
!     Points uniformly distributed in the sense
!     that the expected number of points in a
!     region of the block depends only on the
!     region's area
!     Coordinates from 0 to length and 0 to width
!
subroutine random_2d_block( x, y, length, width )
    real(kind=wp), dimension(:), intent(inout) :: x
    real(kind=wp), dimension(:), intent(inout) :: y
    real(kind=wp), intent(in)                  :: length
    real(kind=wp), intent(in)                  :: width

    integer                                    :: i
    integer                                    :: n
    real(kind=wp)                              :: range

    if ( length <= 0.0 ) return
    if ( width  <= 0.0 ) return

    range = max( length, width )

    i = 0
    n = size(x)

    do while ( i < n )
        call random_number( x(i) )
        call random_number( y(i) )
        x(i) = range * x(i)
        y(i) = range * y(i)
        if ( x(i) <= length .and. y(i) <= width ) then
            i = i + 1
        endif
    enddo
end subroutine random_2d_block

! random_2d_block_filter --
!     Generate points within a two-dimensional block
!     that are uniformly distributed
! Arguments:
!     x       Array of x-coordinates to be filled
!     y       Array of y-coordinates to be filled
!     length  Length of the block
!     width   Width of the block
!     filter  Filter function
!     params  Array of parameters for the filter
! Result:
!     The points are generated as in random_2d_block
!     but are passed to the filter function as well,
!     only if they pass the filter are they accepted
!
subroutine random_2d_block_filter( x, y, length, width, filter, params )
    real(kind=wp), dimension(:), intent(inout) :: x
    real(kind=wp), dimension(:), intent(inout) :: y
    real(kind=wp), intent(in)                  :: length
    real(kind=wp), intent(in)                  :: width
    real(kind=wp), dimension(:), intent(in)    :: params

    interface
        logical function filter( xp, yp, x, y, params )
            use select_precision
            real(kind=wp), intent(inout)            :: xp
            real(kind=wp), intent(inout)            :: yp
            real(kind=wp), dimension(:), intent(in) :: x
            real(kind=wp), dimension(:), intent(in) :: y
            real(kind=wp), dimension(:), intent(in) :: params
        end function filter
    end interface

    integer                                    :: i
    integer                                    :: n
    real(kind=wp)                              :: range

    if ( length <= 0.0 ) return
    if ( width  <= 0.0 ) return

    range = max( length, width )

    i = 0
    n = size(x)

    do while ( i < n )
        call random_number( x(i) )
        call random_number( y(i) )
        x(i) = range * x(i)
        y(i) = range * y(i)
        if ( x(i) <= length .and. y(i) <= width .and. &
            filter( x(i), y(i), x(1:i-1), y(1:i-1), params ) ) then
            i = i + 1
        endif
    enddo
end subroutine random_2d_block_filter

! filter_triangle --
!     Filter points that do not lie in a triangle
! Arguments:
!     xp      X-coordinate of candidate point
!     yp      Y-coordinate of candidate point
!     x       Array of x-coordinates filled so far
!     y       Array of y-coordinates filled so far
!     params  Array of parameters for the filter
! Result:
!     True if the point lies within the triangle,
!     false otherwise
!
logical function filter( xp, yp, x, y, params )
    use select_precision
    real(kind=wp), intent(inout)            :: xp
    real(kind=wp), intent(inout)            :: yp
    real(kind=wp), dimension(:), intent(in) :: x
    real(kind=wp), dimension(:), intent(in) :: y
    real(kind=wp), dimension(:), intent(in) :: params

    if ( xp/params(1)+yp/params(2) < 1.0 ) then
        filter = .true.
    else
        filter = .false.
    endif
end function filter

! random_2d_block_triangle --
!     Generate points within the triangle (0,0)-(length,0)-(0,width)
!     that are uniformly distributed
! Arguments:
!     x       Array of x-coordinates to be filled
!     y       Array of y-coordinates to be filled
!     length  Length of the block
!     width   Width of the block
! Result:
!     Uniformly random points
!
subroutine random_2d_block_triangle( x, y, length, width )
    real(kind=wp), dimension(:), intent(inout) :: x
    real(kind=wp), dimension(:), intent(inout) :: y
    real(kind=wp), intent(in)                  :: length
    real(kind=wp), intent(in)                  :: width

    real(kind=wp), dimension(1:2)              :: params

    params(1) = length
    params(2) = width

    call random_2d_block_filter( x, y, length, width, filter_triangle, params )
end subroutine random_2d_block_triangle

end module pointsets
