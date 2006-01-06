! test_dict.f90 --
!     Test program for dictionaries
!
module MYDATA_MODULE

!
! The length of the keys
!
integer, parameter :: DICT_KEY_LENGTH = 20

!
! The data that will be stored with each key
type MYDATA
    character(len=20) :: string
end type MYDATA

!
! The "null" value for these data
!
type(MYDATA), parameter :: DICT_NULL = mydata( '' )

end module

module MYDATA_DICTS
    use MYDATA_MODULE, DICT_DATA => MYDATA

    include "dictionary.f90"
end module MYDATA_DICTS

program test_dict
    use MYDATA_DICTS

    implicit none

    type(DICT_STRUCT), pointer     :: dict
    type(DICT_DATA)                :: data
    character(len=DICT_KEY_LENGTH) :: key

    !
    ! Create a small dictionary
    !
    data%string = 'Xylophone'
    call dict_create( dict, 'X', data )

    data%string = 'Qi'
    call dict_add_key( dict, 'Q', data )

    data%string = 'Piano-forte'
    call dict_add_key( dict, 'P', data )


    !
    ! Retrieve a particular key ...
    !
    write(*,*) 'Has "A"? ', dict_has_key(dict, 'A' )
    write(*,*) 'Has "Q"? ', dict_has_key(dict, 'Q' )

    !
    ! Retrieve the value for 'P'
    !
    write(*,*) 'P = ', dict_get_key(dict, 'P' )

    !
    ! Remove that key and check again
    !
    call dict_delete_key( dict, "P" )
    write(*,*) 'Still has "P"? ', dict_has_key(dict, 'P' )
    !
    ! Destroy the dictionary
    !
    call dict_destroy( dict )

end program
