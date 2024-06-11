! main_filter_array.f90 --
!     Demonstration of how the filtering works for array-based collections
!
program demo_filter
    use m_collection_real_array

    implicit none

    type(collection_array)          :: array
    type(data_type), dimension(100) :: array_data
    type(data_type)                 :: item

    real(real_kind)                 :: minimum
    real(real_kind), dimension(100) :: random_value
    logical                         :: retrieved

    !
    ! Set up the collection and the filter
    !
    call random_number( random_value )

    array_data = random_value      ! We need a transformation or
                                   ! a generic method

    call array%create( array_data )

    minimum = 0.6_real_kind
    call array%set_filter( is_greater )

    !
    ! Traverse the collection
    !
    do while ( array%has_next() )
        call array%get( item, retrieved )
        write(*,*) item%value
    enddo

contains

! is_greater --
!     Check if the real value is larger than the set minimum
!
logical function is_greater( value )
    type(data_type), intent(in) :: value

    is_greater = value%value > minimum
end function is_greater
end program demo_filter
