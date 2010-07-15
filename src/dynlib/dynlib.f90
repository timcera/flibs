! dynlib.f90 --
!     Module to handle procedures from dynamic libraries
!     Requires Fortran 2003
!
!     Note:
!     There are two implementations of the basic routines,
!     one for Windows and one for Linux/OSX, as the
!     underlying system routines have different APIs
!
!     TODO:
!     Register the contents of an internal library
!     and retrieving that
!
!     TODO:
!     Handle module names
!
module dynamic_libraries
    use iso_c_binding
    use system_dynamic_libraries

    implicit none

    private

    type dynamic_library
        private
        integer(kind=c_long) :: handle   = 0
        logical              :: loaded   = .false.
        logical              :: internal = .false.
        character(len=512)   :: name
    end type dynamic_library

    public :: dynamic_library
    public :: load_library
    public :: get_procedure
    !public :: register_internal_library
    !public :: register_procedure

contains

! load_library --
!     Load a dynamic library (DLL/SO)
!
! Arguments:
!     dynlib            Variable holding the handle to the library
!     name              Name of the library to load
!     success           Whether it was loaded successfully or not
!
subroutine load_library( dynlib, name, success )

    type(dynamic_library), intent(inout) :: dynlib
    character(len=*), intent(in)           :: name
    logical, intent(out)                   :: success

    character(kind=c_char), dimension(512) :: cname
    integer                                :: i

    success = .false.

    write(*,*) 'In load_library'
    dynlib%loaded   = .false.
    dynlib%internal = .false.
    dynlib%name     = name

    cname(1:len(name)+1) = (/ ( name(i:i), i = 1,len(name) ), char(0) /)

    write(*,*) 'Before system_load_library'
    call system_load_library( dynlib%handle, cname )

    write(*,*) 'After system_load_library'
    if ( dynlib%handle /= 0_c_long ) then
        dynlib%loaded = .true.
        success = .true.
    endif
end subroutine load_library

! get_procedure --
!     Get a procedure pointer from a loaded dynamic library (DLL/SO)
!
! Arguments:
!     dynlib            Variable holding the handle to the library
!     name              Name of the procedure to get
!     proc              Pointer to the procedure
!     success           Whether it was loaded successfully or not
!
subroutine get_procedure( dynlib, name, proc, success )

    type(dynamic_library), intent(inout)   :: dynlib
    character(len=*), intent(in)           :: name
    procedure(), pointer                   :: proc
    logical, intent(out)                   :: success

    character(kind=c_char), dimension(512) :: cname
    type(c_funptr)                         :: cproc
    integer                                :: i

    success = .false.
    proc    => null()

    if ( .not. dynlib%loaded ) then
        return
    endif

    if ( .not. dynlib%internal ) then
        cname(1:len(name)+1) = (/ ( name(i:i), i = 1,len(name) ), char(0) /)

        call system_get_procedure( dynlib%handle, cname, cproc, success )

        if ( success  ) then
            call c_f_procpointer( cproc, proc )
        endif
    else
        ! TODO
    endif
end subroutine get_procedure

end module dynamic_libraries
