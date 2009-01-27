! testcov_reg.f90
!     Public routines and module for test coverage
!     Belongs to instrumentation/reporting in testcov.f90
!
module testcov_reg
    implicit none

    type data_per_file
        character(len=200)             :: filename
        integer, dimension(:), pointer :: count
    end type

    type(data_per_file), pointer, dimension(:), save :: coverage

contains

! testcov_load
!     Load an existing test coverage file
!
subroutine testcov_load
    integer :: i
    integer :: lu
    integer :: records
    integer :: filesize

    logical :: exists
    logical :: opend

    inquire( file = 'testcov.data', exist = exists )

    if ( exists ) then
        do i = 10,99
            inquire( unit = i, opened = opend )
            if ( .not. opend ) then
                lu = i
                exit
            endif
        enddo
        open( lu, file = 'testcov.data', form = 'unformatted' )
        read( lu ) records

        allocate( coverage(records) )

        do i = 1,records
            read( lu ) coverage(i)%filename, filesize
            allocate( coverage(i)%count(filesize) )
            read( lu ) coverage(i)%count
        enddo
        close( lu )
    endif

end subroutine testcov_load

! testcov_dump
!     Dump the coverage data to file
!
subroutine testcov_dump
    integer :: i
    integer :: lu
    integer :: records

    logical :: opend

    do i = 10,99
        inquire( unit = i, opened = opend )
        if ( .not. opend ) then
            lu = i
            exit
        endif
    enddo

    open( lu, file = 'testcov.data', form = 'unformatted' )
    write( lu ) records

    do i = 1,records
        write( lu ) coverage(i)%filename, size(coverage(i)%count)
        write( lu ) coverage(i)%count
    enddo
    close( lu )

end subroutine testcov_dump

! testcov_make_available
!     Make a record and the line entry available
!
! Arguments:
!     filename          Name of the source file
!     lineno            Line number in the source file
!     idx               Index in the coverage array
!
subroutine testcov_make_available( filename, lineno, idx )
    character(len=*), intent(in)   :: filename
    integer, intent(in)            :: lineno
    integer, intent(out)           :: idx

    integer                        :: i
    integer                        :: oldsize
    integer, dimension(:), pointer :: newcount
    type(data_per_file), dimension(:), pointer :: newcoverage

    integer, save                  :: old_index = -1

    if ( .not. associated(coverage) ) then
        allocate( coverage(0) )
    endif

    !
    ! Cached index may apply
    !
    if ( old_index > 0 ) then
        if ( coverage(old_index)%filename == filename ) then
            idx = old_index
            return
        endif
    endif

    !
    ! Does the record already exist? If so, check the size
    !
    do i = 1,size(coverage)
        if ( coverage(i)%filename == filename ) then
            if ( size(coverage(i)%count) < 3*lineno ) then
                oldsize = size(coverage(i)%count)

                allocate( newcount(3*lineno) )

                newcount            = 0
                newcount(1:oldsize) = coverage(i)%count
                deallocate( coverage(i)%count )
                coverage(i)%count   => newcount
            endif

            idx = i
            old_index = idx
            return
        endif
    enddo

    !
    ! A new record is required
    !
    allocate( newcoverage(1+size(coverage)) )

    idx = size( newcoverage )

    newcoverage(1:idx-1) = coverage
    deallocate( coverage )
    coverage => newcoverage

    coverage(idx)%filename = filename

    allocate( coverage(idx)%count(3*lineno) )
    coverage(idx)%count = 0

    old_index = idx

end subroutine testcov_make_available

end module

! testcov_register__ --
!     Register the test coverage in an instrumented program
!
! Arguments:
!     filename           Name of the original source file
!     lineno             Line number in the original source file
!     type               Type of registration
!
subroutine testcov_register__( filename, lineno, type )
    use testcov_reg

    character(len=*) :: filename  ! Note: no intent(in) - FORTRAN 77 style required!
    integer          :: lineno
    integer          :: type

    integer          :: idx
    logical, save    :: init = .true.

    if ( init ) then
        init = .false.
        call testcov_load
    endif

    call testcov_make_available( filename, lineno, idx )

    select case( type )
        case( 0 ) ! Dump the results
            call testcov_dump

        case( 1 ) ! Normal case
            coverage(idx)%count(3*lineno-1) = coverage(idx)%count(3*lineno-1) + 1

        case( 2 ) ! Prepare do-loop
            coverage(idx)%count(3*lineno-2) = 0 ! "local" counter
            coverage(idx)%count(3*lineno-1) = coverage(idx)%count(3*lineno-2) + 1

        case( 3 ) ! End of do-loop
            if ( coverage(idx)%count(3*lineno-2) == 0 ) then
                coverage(idx)%count(3*lineno) = coverage(idx)%count(3*lineno) + 1
            endif

        case( 4 ) ! Implicit branch for if/select case
            coverage(idx)%count(3*lineno) = coverage(idx)%count(3*lineno) + 1
    end select
end subroutine
