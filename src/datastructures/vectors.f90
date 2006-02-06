! vecrtors.f90 --
!     Implementation of dynamically growing arrays
!
!     See the example/test program for the way to use this
!
!     The name has been chosen from the Standard Template
!     Library known from C++. Java has a similar class.
!
!     The module is straightforward: it defines a suitable
!     data structure, data can be added to the vector
!     and you can retrieve data from it.
!
!     Note:
!     For the function vector_at() we need a parameter
!     that represents the "empty vector data" value.

type VECTOR
    integer, private                         :: no_used
    type(VECTOR_DATA), dimension(:), pointer :: data    => null()
end type VECTOR

private
public :: VECTOR
public :: vector_create, vector_append, vector_at, &
          vector_size, vector_put, vector_delete_element, &
          vector_destroy

real, parameter :: growth_rate = 1.1

contains

! vector_create
!     Create a new vector
!
! Arguments:
!     vector        Variable that should hold the vector
!     capacity      Initial capacity (optional)
!
! Note:
!     The fields of the vector data structure are set
!
subroutine vector_create( vector, capacity )
    type(VECTOR)       :: vector
    integer, optional  :: capacity

    integer            :: cap

    !
    ! Check that the vector does not have any data left
    !
    if ( associated(vector%data) ) then
        call vector_destroy( vector )
    endif

    if ( present(capacity) ) then
        cap = max( 1, capacity )
    else
        cap = 10
    endif
    allocate( vector%data(1:cap) )
    vector%no_used = 0
end subroutine vector_create

! vector_destroy
!     Destroy a vector
!
! Arguments:
!     vector        Vector in question
!
subroutine vector_destroy( vector )
    type(VECTOR)       :: vector

    !
    ! Check that the vector does not have any data left
    !
    if ( associated(vector%data) ) then
        deallocate( vector%data )
    endif
    vector%no_used = 0
end subroutine vector_destroy

! vector_at
!     Get the value of the nth element of the vector
!
! Arguments:
!     vector        Vector in question
!     n             Index of the element whose value
!                   should be retrieved
!
type(VECTOR_DATA) vector_at( vector, n )
    type(VECTOR)       :: vector
    integer            :: n

    if ( n .lt. 1 .or. n .gt. vector%no_used ) then
        vector_at = empty_vector_data
    else
        vector_at = vector%data(n)
    endif
end function vector_at

! vector_insert_empty
!     Insert one or more empty elements
!
! Arguments:
!     vector        Vector in question
!     pos           Position to insert the empty elements
!     number        Number of empty elements
!
subroutine vector_insert_empty( vector, pos, number )
    type(VECTOR)         :: vector
    integer, intent(in)  :: pos
    integer, intent(in)  :: number

    integer              :: i

    if ( number .lt. 1 .or. pos .lt. 1 .or. pos .gt. vector%no_used ) then
        return
    endif

    if ( vector%no_used+number .ge. size(vector%data) ) then
        call vector_increase_capacity( vector, vector%no_used+number )
    endif

    do i = 1,number
        vector%data(vector%no_used+i-1) = vector%data(pos+i-1)
    enddo

    do i = 1,number
        vector%data(pos+i-1) = empty_vector_data
    enddo

    vector%no_used = vector%no_used + number
end subroutine vector_insert_empty

! vector_append
!     Append a value to the vector
!
! Arguments:
!     vector        Vector in question
!     data          Data to be appended
!
subroutine vector_append( vector, data )
    type(VECTOR)       :: vector
    type(VECTOR_DATA)  :: data

    if ( vector%no_used .ge. size(vector%data) ) then
        call vector_increase_capacity( vector, vector%no_used+1 )
    endif

    vector%no_used = vector%no_used + 1
    vector%data(vector%no_used) = data
end subroutine vector_append

! vector_put
!     Put a value at a specific element of the vector
!     (it needs not yet exist)
!
! Arguments:
!     vector        Vector in question
!     n             Index of the element
!     data          Data to be put in the vector
!
subroutine vector_put( vector, n, data )
    type(VECTOR)       :: vector
    integer            :: n
    type(VECTOR_DATA)  :: data

    if ( n .gt. size(vector%data) ) then
        call vector_increase_capacity( vector, n )
    endif

    vector%no_used = n
    vector%data(vector%no_used) = data
end subroutine vector_put

! vector_increase_capacity
!     Expand the array holding the data
!
! Arguments:
!     vector        Vector in question
!     capacity      Minimum capacity
!
subroutine vector_increase_capacity( vector, capacity )
    type(VECTOR)       :: vector
    integer            :: capacity

    integer            :: new_cap
    type(VECTOR_DATA), dimension(:), pointer :: new_data

    new_cap = max( capacity, nint( growth_rate * size(vector%data) ) )

    if ( new_cap .gt. size(vector%data) ) then
        allocate( new_data(1:new_cap) )
        new_data(1:vector%no+used) = vector%data(1:vector%no_used)
        new_data(vector%no_used+1:new_cap) = empty_vector_data

        deallocate( vector%data )
        vector%data => new_data
    endif

end subroutine vector_capacity
