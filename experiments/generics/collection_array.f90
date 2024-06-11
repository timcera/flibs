! collection_array.f90 --
!     Modules defining several types of array-based collections
!
module m_collection_real_array
    use basic_types, only: data_type => real_type, assignment(=)

    private
    public   :: collection_array, data_type, real_kind, assignment(=)

    integer, parameter :: real_kind = kind(1.0)

include 'collection_array_body.f90'

end module m_collection_real_array

module m_collection_double_array
    use basic_types, only: data_type => double_type, assignment(=)

    private
    public   :: collection_array, data_type, real_kind, assignment(=)

    integer, parameter :: real_kind = kind(1.0d0)

include 'collection_array_body.f90'

end module m_collection_double_array

module m_collection_integer_array
    use basic_types, only: data_type => integer_type, assignment(=)

    private
    public   :: collection_array, data_type, assignment(=)

include 'collection_array_body.f90'

end module m_collection_integer_array
