! ftnunit_store.f90 --
!     Module to store and retrieve arrays of data for the
!     "ftnunit" framework:
!     Quite often numerical routines require a lot of input
!     data and produce a lot of output data. This module
!     provides routines to store such data in an external
!     file and to retrieve them again at testing time.
!     The files it handles are simple unformatted files,
!     but they hold enough information to accomplish
!     the storage and retrieval:
!     - A header identifying the type of file
!     - A short description of the contents
!     - Per array:
!       - A record with metadata (data type and array dimensions)
!       - A record with the actual data
!
module ftnunit_store
    use ftnunit_utilities

    implicit none

    private
    integer, parameter :: single = kind(1.0)
    integer, parameter :: double = kind(1.0d)

    character(len=40), parameter :: ftnunit_header    = 'FTNUNIT 1.0'
    character(len=10), parameter :: ftnunit_integer   = 'INTEGER'
    character(len=10), parameter :: ftnunit_logical   = 'LOGICAL'
    character(len=10), parameter :: ftnunit_real      = 'REAL'
    character(len=10), parameter :: ftnunit_double    = 'DOUBLE'
    character(len=10), parameter :: ftnunit_character = 'CHARACTER'
    character(len=10), parameter :: ftnunit_complex   = 'COMPLEX'

    interface test_retrieve_data
        module procedure test_retrieve_data_int
        module procedure test_retrieve_data_int1d
        module procedure test_retrieve_data_int2d
        module procedure test_retrieve_data_int3d
        ! ...
    end interface

    interface test_store_data
        module procedure test_store_data_int
        module procedure test_store_data_int1d
        module procedure test_store_data_int2d
        module procedure test_store_data_int3d
        ! ...
    end interface

    public :: test_retrieve_data, test_store_data
    public :: test_open_storage_file, test_close_storage_file
    ! public :: test_print_storage_file_summary

contains

! test_open_storage_file -
!     Open a new or existing storage file
!
! Arguments:
!     filename       Name of the file (input)
!     lun            LU-number of the opened file (output)
!     desc           Short description (input/output)
!                    (At most 80 characters)
!     output         Whether to open for output or not
!                    (Optional; default: input)
!
! Note:
!     The routine is rather fussy about these files:
!     An existing file will not be overwritten and a
!     file with the wrong contents is refused.
!
subroutine test_open_storage_file( filename, lun, desc, output )
    character(len=*), intent(in)    :: filename
    integer, intent(out)            :: lun
    character(len=*), intent(inout) :: desc
    logical, intent(in), optional   :: output

    logical                         :: output_
    character(len=80)               :: desc_
    character(len=40)               :: header
    integer                         :: ierr

    output_ = .false.
    if ( present(output) ) then
        output_ = output
    endif

    !
    ! Get a LU-number, open the file and check
    !
    call ftnunit_get_lun( lun )

    if ( output_ ) then
        open( lun, file = filename, form = 'unformatted', status = 'new', iostat = ierr )

        if ( ierr /= 0 ) then
            write(*,*) 'FTNUNIT: unable to open file "' // trim(filename) // ' for writing'
            write(*,*) '    It should not exist yet - please check'
            stop
        endif

        write( lun ) ftnunit_header
        desc_ = desc
        write( lun ) desc_  ! Ensure a known length of the description

    else
        open( lun, file = filename, form = 'unformatted', status = 'old', iostat = ierr )

        if ( ierr /= 0 ) then
            write(*,*) 'FTNUNIT: unable to open file "' // trim(filename) // ' for reading'
            write(*,*) '    It should exist - please check'
            stop
        endif

        read( lun, iostat = ierr ) header

        if ( ierr /= 0 ) then
            write(*,*) 'FTNUNIT: corrupt file "' // trim(filename)
            write(*,*) '    Unable to read the header - please check'
            stop
        endif

        if ( header /= ftnunit_header ) then
            write(*,*) 'FTNUNIT: incorrect file "' // trim(filename)
            write(*,*) '    It is not a valid ftnunit storage file - please check'
            stop
        endif

        read( lun, iostat = ierr ) desc_  ! Ensure a known length of the description

        if ( ierr /= 0 ) then
            write(*,*) 'FTNUNIT: corrupt file "' // trim(filename)
            write(*,*) '    Unable to read the description - please check'
            stop
        endif

        desc = desc_
    endif

end subroutine test_open_storage_file


! test_close_storage_file -
!     Close the storage file
!
! Arguments:
!     lun            LU-number of the opened file (output)
!
! Note:
!     Just for symmetry
!
subroutine test_close_storage_file( lun )
    integer, intent(in)             :: lun

    close( lun )

end subroutine test_close_storage_file


! test_store_data_integer* -
!     Store integer data
!
! Arguments:
!     lun            LU-number of the opened file
!     data           Data to store
!     desc           Short description (at most 40 long)
!
!
subroutine test_store_data_integer( lun, data, desc )
    integer, intent(in)             :: lun
    integer, intent(in)             :: data
    character(len=*)                :: desc

    character(len=40)               :: desc_
    integer, dimension(10)          :: dimensions

    desc_ = desc
    dimensions = 0

                                  !char length, #dimensions
    write( lun ) ftnunit_integer, 0, 0, dimensions, desc_
    write( lun ) data

    close( lun )

end subroutine test_store_data_integer

subroutine test_store_data_integer1d( lun, data, desc )
    integer, intent(in)               :: lun
    integer, intent(in), dimension(:) :: data
    character(len=*)                  :: desc

    character(len=40)                 :: desc_
    integer, dimension(10)            :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:1) = shape(data)

    write( lun ) ftnunit_integer, 0, 1, dimensions, desc_
    write( lun ) data

    close( lun )

end subroutine test_store_data_integer1d

subroutine test_store_data_integer2d( lun, data, desc )
    integer, intent(in)                 :: lun
    integer, intent(in), dimension(:,:) :: data
    character(len=*)                    :: desc

    character(len=40)                   :: desc_
    integer, dimension(10)              :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:2) = shape(data)

    write( lun ) ftnunit_integer, 0, 2, dimensions, desc_
    write( lun ) data

    close( lun )

end subroutine test_store_data_integer2d

subroutine test_store_data_integer3d( lun, data, desc )
    integer, intent(in)                   :: lun
    integer, intent(in), dimension(:,:,:) :: data
    character(len=*)                      :: desc

    character(len=40)                     :: desc_
    integer, dimension(10)                :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:3) = shape(data)

    write( lun ) ftnunit_integer, 0, 3, dimensions, desc_
    write( lun ) data

    close( lun )

end subroutine test_store_data_integer3d


end module ftnunit_storage
