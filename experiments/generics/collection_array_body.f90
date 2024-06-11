! collection_array_body.f90 --
!     Include containing the complete declaration and implementation
!     of file-based collections
!
    include "collection_generic.f90"

    type, extends(collection) :: collection_array
        type(data_type), dimension(:), allocatable :: array
        integer                                    :: index
    contains
        procedure :: create     => create_array
        procedure :: get_next   => get_next_array
    end type collection_array

contains

include 'collection_implementation.f90'


! create_array --
!     Create the object
!
! Arguments:
!     this              The container object - holds the data and has knowledge of where we are
!
subroutine create_array( this, array )
    class(collection_array), intent(inout) :: this
    type(data_type), dimension(:)          :: array

    this%index       = 0
    this%array       = array
    this%initialised = .true.
end subroutine create_array

! get_next_array --
!     Get the next item from array container
!
! Arguments:
!     this              The container object - holds the data and has knowledge of where we are
!     item              The item that is retrieved
!     retrieved         Indicates if an item was retrieved or not
!
subroutine get_next_array( this, item, retrieved )
    class(collection_array), intent(inout) :: this  ! The container may have to update the state
    type(data_type), intent(inout)         :: item
    logical, intent(out)                   :: retrieved

    if ( this%initialised .and. this%index <= size(this%array) ) then
        item       = this%array(this%index)
        retrieved  = .true.
    else
        retrieved  = .false.
    endif

    this%index = this%index + 1
end subroutine get_next_array
