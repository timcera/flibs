! collection_file.f90 --
!     Modules defining a two types of file-based collections
!
module m_collection_file
    use basic_types, only: data_type => string_type, assignment(=)

    private
    public  :: collection_file, data_type

include 'collection_file_body.f90'

end module m_collection_file

module m_collection_station_data
    use basic_types, only: data_type => station_data_type, assignment(=)

    private
    public  :: collection_file, data_type

include 'collection_file_body.f90'

end module m_collection_station_data
