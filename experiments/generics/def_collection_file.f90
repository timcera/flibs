! def_collection_file.f90 --
!     Use a hypothetical extension to the Fortran syntax to make using templates
!     easier
!
!     Some practical difficulties:
!     - a flexible-length string: how to do that? Similarly for any "compound" basic type
!     - using "implicit none" in a template
!
template collection_generic
    implicit none ! Useful? - should be eliminated

    type, abstract :: collection
        logical                                  :: initialised = .false.  ! State: not initialised, initialised (data), has filter, has next
        logical                                  :: has_filter  = .false.
        logical                                  :: next_item   = .false.
        type(data_type)                          :: item
        procedure(filter), pointer, nopass       :: acceptable  => null()
    contains
        procedure                                :: has_next   => has_next_generic
        procedure                                :: get        => get_generic
        procedure                                :: set_filter => set_filter_generic
        procedure(get_next_item), deferred, pass :: get_next
    end type collection

    abstract interface
        logical function filter( item )
            import                      :: data_type
            type(data_type), intent(in) :: item
        end function filter

        subroutine get_next_item( this, item, retrieved )
            import                           :: collection, data_type
            class(collection), intent(inout) :: this
            type(data_type), intent(inout)   :: item
            logical, intent(out)             :: retrieved
        end subroutine
    end interface
contains

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

end template collection_generic



template collection_file_def
    implicit none
    use_template collection_generic

    type, extends(collection) :: collection_file
        integer         :: lun
    contains
        procedure                :: create   => create_file
        procedure                :: get_next => get_next_file
    end type collection_file

contains

! create_file --
!     Open the file that is to serve as the container
!
! Arguments:
!     this              The container object - holds the data and has knowledge of where we are
!     skiplines         Number of lines to be skipped
!
subroutine create_file( this, filename, skiplines )
    class(collection_file), intent(inout) :: this
    character(len=*), intent(in)          :: filename
    integer, intent(in), optional         :: skiplines

    integer                               :: i

    open( newunit = this%lun, file = filename )

    if ( present(skiplines) ) then
        do i = 1,skiplines
            read( this%lun, * )
        enddo
    endif

    this%initialised = .true.

end subroutine create_file

! get_next_file --
!     Get the next item - any line will do
!
! Arguments:
!     this              The container object - holds the data and has knowledge of where we are
!     item              The item that is retrieved
!     retrieved         Indicates if an item was retrieved or not
!
! Note:
!     This routine is actually specific to the data type! So it must be specialised in
!     accordance
!
subroutine get_next_file( this, item, retrieved )
    class(collection_file), intent(inout) :: this  ! The container may have to update the state
    type(data_type), intent(inout)         :: item
    logical, intent(out)                   :: retrieved

    character(len=100)                     :: line ! Rather arbitrary length, could use the flexible routine read_line_from_file
    integer                                :: ierr

    read( this%lun, '(a)', iostat = ierr ) line

    if ( ierr == 0 ) then
        retrieved = .true.
        item      = trim(line)
    else
        retrieved = .false.
    endif
end subroutine get_next_file

end template collection_file_def



module m_collection_file
    use basic_types, only: string_type, assignment(=)
    use_template collection_file_def, string_type => data_type

    private
    public  :: collection_file, string_type
end module m_collection_file

!
! Alternative: ordinary strings
!
module m_collection_file_string
    use_template collection_file_def, character(len=:) => data_type  ! This should have the "allocatable" attribute as appropriate

    !
    ! Maybe this alternative:
    ! use_template collection_file_def, (character(len=:), allocatable) => data_type

    private
    public  :: collection_file
end module m_collection_file_string

