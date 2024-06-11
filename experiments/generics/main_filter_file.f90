! main_filter_file.f90 --
!     Demonstration of how the filtering works for file-based collections
!
!     Note:
!     - get() is to be used in conjunction with has_next()
!     - get_next() can be used to simply get the next line in the file
!       - without the filter
!     - we use a defined assignment to copy the contents of the
!       line that is read to the type(data_type) variable contained
!       in the collection.
!
!     Imported type:
!     type string_type
!         character(len=:), allocatable :: value
!     end type string_type
!
program demo_filter
    use m_collection_file

    implicit none

    type(collection_file)           :: file
    type(data_type)                 :: item
    character(len=20)               :: pattern
    logical                         :: retrieved

    !
    ! Set up the collection and the filter
    !
    call file%create( 'somedata.csv', skiplines = 1 )

    pattern = "NW1"
    call file%set_filter( contains_pattern )

    !
    ! Traverse the collection
    !
    do while ( file%has_next() )
        call file%get( item, retrieved )
        write(*,*) item%value
    enddo

contains

! contains_pattern --
!     Check if the string contains the pattern
!
logical function contains_pattern( string )
    type(data_type), intent(in) :: string

    contains_pattern = ( index( string%value, trim(pattern) ) > 0 )
end function contains_pattern
end program demo_filter
