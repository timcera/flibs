! basic_types.f90 --
!     Define some basic types for use in the collection classes
!     - demonstrate a straightforward approach to "generic" programming
!
!     There is no problem importing derived types in a generic way.
!     Use the rename facility: use some_module, specific_type => general_name
!
module basic_types
    implicit none

    type integer_type
        integer :: value
    end type integer_type

    type real_type
        real :: value
    end type real_type

    type double_type
        real(kind=kind(1.0d0)) :: value
    end type double_type

    type string_type
        character(len=:), allocatable :: value
    end type string_type

    type station_data_type
        character(len=20) :: station
        character(len=10) :: date
        real              :: salinity
        real              :: temperature
    end type station_data_type

    interface assignment(=)
        module procedure assign_string
        module procedure assign_station_data
        module procedure assign_sp_real
        module procedure assign_dp_real
        module procedure assign_integer
    end interface

contains

! assign_integer --
!     Copy the contents of an ordinary integer to a type(integer_type) variable
!
! Arguments:
!     value               The type(integer_type) variable in question
!     intvalue            Ordinary integer
!
elemental subroutine assign_integer( value, intvalue )
    type(integer_type), intent(inout) :: value
    integer, intent(in)               :: intvalue

    value%value = intvalue
end subroutine assign_integer

! assign_sp_real --
!     Copy the contents of an ordinary single-precision real to a type(real_type) variable
!
! Arguments:
!     value               The type(real_type) variable in question
!     spvalue             Ordinary single-precision real
!
elemental subroutine assign_sp_real( value, spvalue )
    type(real_type), intent(inout) :: value
    real, intent(in)               :: spvalue

    value%value = spvalue
end subroutine assign_sp_real

! assign_dp_real --
!     Copy the contents of an ordinary double-precision real to a type(double_type) variable
!
! Arguments:
!     value               The type(double_type) variable in question
!     dpvalue             Ordinary double-precision real
!
elemental subroutine assign_dp_real( value, dpvalue )
    type(double_type), intent(inout)   :: value
    real(kind=kind(1.0d0)), intent(in) :: dpvalue

    value%value = dpvalue
end subroutine assign_dp_real

! assign_string --
!     Copy the contents of an ordinary string to a type(string_type) variable
!
! Arguments:
!     string              The type(string_type) variable in question
!     char                Ordinary character string
!
subroutine assign_string( string, char )
    type(string_type), intent(inout) :: string
    character(len=*), intent(in)     :: char

    string%value = char
end subroutine assign_string

! assign_station_data --
!     Copy the contents of a CSV string to a type(station_data_type) variable
!
! Arguments:
!     string              The type(string_type) variable in question
!     char                Ordinary character string
!
subroutine assign_station_data( station_data, char )
    type(station_data_type), intent(inout) :: station_data
    character(len=*), intent(in)     :: char

    read( char, * ) station_data%station, station_data%date, station_data%salinity, station_data%temperature
end subroutine assign_station_data

subroutine testje
    type(real_type) :: v

    v = 1.0
end subroutine testje

end module basic_types
