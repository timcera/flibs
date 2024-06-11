! collection_implementation.f90 --
!     Provide the implementation of the generic routines in the generic collection class:
!
!     Note:
!     This source file is meant to be included
!

! set_filter_generic --
!     Register the filter function
!
! Arguments:
!     this              The collection object - holds the data and has knowledge of where we are
!     filter_ptr        Pointer to the function to be used as filter
!
subroutine set_filter_generic( this, filter )
    class(collection), intent(inout) :: this

    interface
        logical function filter( item )
            import                      :: data_type
            type(data_type), intent(in) :: item
        end function filter
    end interface

    this%acceptable => filter
end subroutine set_filter_generic

! has_next_generic --
!     Check if there is another acceptable element in the collection
!
! Arguments:
!     this              The collection object - holds the data and has knowledge of where we are
!
! Result:
!     True if there is such an element (this will have been retrieved!), fals otherwise.
!
logical function has_next_generic( this )
    class(collection), intent(inout) :: this  ! The container needs to store the element and update the state

    type(data_type)                  :: item
    logical                          :: success

    has_next_generic = .false.
    do
        call this%get_next( item, success )

        if ( .not. success ) then
            this%next_item   = .false.
            exit
        endif
        if ( this%acceptable( item ) ) then
            has_next_generic = .true.
            this%next_item   = .true.
            this%item        = item
            exit
        endif
    enddo
end function has_next_generic

! get_generic --
!     Get the next (acceptable) item
!
! Arguments:
!     this              The container object - holds the data and has knowledge of where we are
!     item              The item that is retrieved
!     retrieved         Indicates if an item was retrieved or not
!
! Note:
!     This routine has to be used in conjunction with has_next()
!
subroutine get_generic( this, item, retrieved )
    class(collection), intent(inout)  :: this  ! The container may have to update the state
    type(data_type), intent(inout)    :: item
    logical, intent(out)              :: retrieved

    if ( this%initialised .and. this%next_item ) then
        item       = this%item
        retrieved  = .true.
    else
        retrieved  = .false.
    endif
end subroutine get_generic
