! main_filter_station.f90 --
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
!     type station_data
!         character(len=20) :: station
!         character(len=20) :: date
!         real              :: salinity
!         real              :: temperature
!     end type station_data
!     The filter needs to be adjusted though
!
program demo_filter
    use m_collection_station_data

    implicit none

    type(collection_file)   :: file
    type(data_type)         :: item
    character(len=20)       :: pattern
    logical                 :: retrieved

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
        write(*,*) item
    enddo

contains

! contains_pattern --
!     Check if the station data for the station "pattern"
!
logical function contains_pattern( station_data )
    type(data_type), intent(in) :: station_data

    contains_pattern = ( station_data%station == pattern )
end function contains_pattern
end program demo_filter
