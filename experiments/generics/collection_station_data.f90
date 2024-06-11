! collections.f90 --
!     Module defining a few types of collections
!
module collections
    !!use basic_types, data_type => string_type
    use basic_types, data_type => station_data_type

    implicit none

    include 'collection_generic.f90'

    type, extends(collection) :: collection_file
        integer         :: lun
    contains
        procedure                :: create   => create_file
        procedure                :: get_next => get_next_file
    end type collection_file

    type, extends(collection_file) :: collection_station_data
    contains
        procedure                :: get_next => get_next_station_data
    end type collection_station_data

contains

include 'collection_implementation.f90'


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

! get_next_station_data --
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
subroutine get_next_station_data( this, item, retrieved )
    class(collection_station_data), intent(inout) :: this  ! The container may have to update the state
    type(data_type), intent(inout)                :: item
    logical, intent(out)                          :: retrieved

    character(len=100)                            :: line ! Rather arbitrary length, could use the flexible routine read_line_from_file
    integer                                       :: ierr

    read( this%lun, '(a)', iostat = ierr ) line

    if ( ierr == 0 ) then
        retrieved = .true.
        item      = trim(line)
    else
        retrieved = .false.
    endif
end subroutine get_next_station_data

end module collections
