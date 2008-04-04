!
! m_platform.f90 --
!     This module provides low-level services to access to the system.
!
!     platform_system                       Executes an external command on the system
!     platform_get_os                       Returns the current operating system
!     platform_get_platform                 Returns the current platform
!     platform_get_environment_variable     Get to one environment variable
!     platform_cd                           Change the system current directory
!     platform_stat                         Get status of a file
!
! Operating System dependency :
! It should be compiled by defining several preprocessing macros, to 
! select from the various operating systems and compilers for which this module
! is made for.
! Choose your OS between one of these :
! _PLATFORM_OS_WINDOWS_95 , 
! _PLATFORM_OS_WINDOWS_NT , 
! _PLATFORM_OS_MAC , 
! _PLATFORM_OS_SUN , 
! _PLATFORM_OS_LINUX , 
! _PLATFORM_OS_UNIX
!
! System fortran extension :
! Depending on the compiler, the SYSTEM fortran extension is provided 
! as a subroutine or a function. See in your manual for the specific 
! settings.
! For example, this is a short list of compilers and their particular 
! SYSTEM provided :
! - subroutine : Intel Fortran, gfortran,
! - function : g95.
! Choose your SYSTEM version between one of these :
! _PLATFORM_SYSTEM_SUBROUTINE , _PLATFORM_SYSTEM_FUNCTION
!
! Environment variables extension :
! The fortran 2003 standard introduces a standard way of accessing
! to the environment variables. Older compilers does not match 
! that standard but provide extensions to access to environment variables.
! Choose your option between one of these :
! _PLATFORM_INTEL_FORTRAN_PORTABILITY_ROUTINES , _PLATFORM_FORTRAN_2003
!
! Change directory fortran extension :
! Depending on the compiler, the "CHDIR" fortran extension is provided 
! as a subroutine or a function. See in your manual for the specific 
! settings.
! For example, this is a short list of compilers and their particular 
! CHDIR provided :
! - function : Intel Fortran, g95, gfortran
! - subroutine : gfortran
! Choose your CHDIR version between one of these :
! _PLATFORM_CHDIR_SUBROUTINE , _PLATFORM_CHDIR_FUNCTION
!
! File stat fortran extension.
! Depending on the compiler the "STAT" fortran extension is 
! provided as a subroutine or a function.
! For example, this is a short list of compilers and their particular 
! STAT provided :
! - subroutine : Intel Fortran, gfortran
! - function : g95
! Choose your STAT version between one of these :
! _PLATFORM_STAT_SUBROUTINE , _PLATFORM_STAT_FUNCTION
!
! This is an abstract of all macros for several compilers.
!
! Compiler : gfortran
! _PLATFORM_FORTRAN_2003
! _PLATFORM_CHDIR_SUBROUTINE
! _PLATFORM_STAT_SUBROUTINE
! _PLATFORM_SYSTEM_SUBROUTINE
!
! Compiler : Intel Fortran
! _PLATFORM_INTEL_FORTRAN_PORTABILITY_ROUTINES
! _PLATFORM_CHDIR_FUNCTION
! _PLATFORM_STAT_FUNCTION
! _PLATFORM_SYSTEM_FUNCTION
!
! Compiler : g95
! _PLATFORM_FORTRAN_2003
! _PLATFORM_CHDIR_FUNCTION
! _PLATFORM_STAT_FUNCTION
! _PLATFORM_SYSTEM_FUNCTION
!
! Copyright (c) 2008 Michaël Baudin
!
module m_platform
#ifdef _PLATFORM_INTEL_FORTRAN_PORTABILITY_ROUTINES
  use ifport
#endif
  implicit none
  private
  !
  ! The name of the operating system running on this machine, such as Windows 95,
  ! Windows NT, MacOS, or SunOS. On UNIX machines, this is the value returned by
  ! uname -s. 
  !
  integer, parameter, public :: PLATFORM_OS_UNKNOWN = 0
  integer, parameter, public :: PLATFORM_OS_WINDOWS_95 = 1
  integer, parameter, public :: PLATFORM_OS_WINDOWS_NT = 2
  integer, parameter, public :: PLATFORM_OS_MACOS = 3
  integer, parameter, public :: PLATFORM_OS_SUNOS = 4
  integer, parameter, public :: PLATFORM_OS_LINUX = 5
  integer, parameter, public :: PLATFORM_OS_UNIX = 6
  integer, parameter, public :: PLATFORM_OS_NB = 6
#ifdef _PLATFORM_OS_WINDOWS_95
  integer, parameter, public :: PLATFORM_OS = PLATFORM_OS_WINDOWS_95
#endif
#ifdef _PLATFORM_OS_WINDOWS_NT
  integer, parameter, public :: PLATFORM_OS = PLATFORM_OS_WINDOWS_NT
#endif
#ifdef _PLATFORM_OS_MAC
  integer, parameter, public :: PLATFORM_OS = PLATFORM_OS_MACOS
#endif
#ifdef _PLATFORM_OS_SUN
  integer, parameter, public :: PLATFORM_OS = PLATFORM_OS_SUNOS
#endif
#ifdef _PLATFORM_OS_LINUX
  integer, parameter, public :: PLATFORM_OS = PLATFORM_OS_LINUX
#endif
#ifdef _PLATFORM_OS_UNIX
  integer, parameter, public :: PLATFORM_OS = PLATFORM_OS_UNIX
#endif
  !
  ! Either windows, macintosh, or unix. This identifies the general operating environment of the machine.
  !
  integer, parameter, public :: PLATFORM_PLATFORM_UNKNOWN = 0
  integer, parameter, public :: PLATFORM_PLATFORM_WINDOWS = 1
  integer, parameter, public :: PLATFORM_PLATFORM_MAC = 2
  integer, parameter, public :: PLATFORM_PLATFORM_UNIX = 3
  integer, parameter, public :: PLATFORM_PLATFORM_NB = 3
#ifdef _PLATFORM_OS_WINDOWS_95
  integer, parameter, public :: PLATFORM_PLATFORM = PLATFORM_PLATFORM_WINDOWS
#endif
#ifdef _PLATFORM_OS_WINDOWS_NT
  integer, parameter, public :: PLATFORM_PLATFORM = PLATFORM_PLATFORM_WINDOWS
#endif
#ifdef _PLATFORM_OS_MAC
  integer, parameter, public :: PLATFORM_PLATFORM = PLATFORM_PLATFORM_MAC
#endif
#ifdef _PLATFORM_OS_SUN
  integer, parameter, public :: PLATFORM_PLATFORM = PLATFORM_PLATFORM_UNIX
#endif
#ifdef _PLATFORM_OS_LINUX
  integer, parameter, public :: PLATFORM_PLATFORM = PLATFORM_PLATFORM_UNIX
#endif
#ifdef _PLATFORM_OS_UNIX
  integer, parameter, public :: PLATFORM_PLATFORM = PLATFORM_PLATFORM_UNIX
#endif
  !
  ! Public static methods
  !
  public :: platform_system
  public :: platform_get_os
  public :: platform_get_platform
  public :: platform_get_environment_variable
  public :: platform_cd
  public :: platform_stat
  public :: platform_osstring
  public :: platform_platformstring
contains
  !
  ! platform_system  --
  !   This subroutine allows to pass commands to the system
  !   and returns the execution status.
  !   Provides an interface to the system fortran extension.
  ! Arguments:
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !       upon return
  !   Under windows, status are unused and therefore allways set to 0.
  ! Caution !
  !   Under Windows the "call system" may generate the display of a console.
  ! Caution !
  !   The standard is that a status different from 0 is an error.
  !   In fact, under Windows, the value of the status has no signification.
  !   I got status 1 or 5228135 with a good copy result.
  !
  subroutine platform_system ( command , status )
    character (len=*), intent(in) :: command
    integer, intent ( out ), optional :: status
    integer :: local_status
    integer :: platform
#ifdef _PLATFORM_SYSTEM_SUBROUTINE
    call system ( command , local_status )
#endif
#ifdef _PLATFORM_SYSTEM_FUNCTION
    local_status = system ( command )
#endif
    if (present(status)) then
       platform = platform_get_platform ()
       if (platform == PLATFORM_PLATFORM_WINDOWS) then
          status = 0
       else
          local_status = status
       endif
    endif
  end subroutine platform_system
  !
  ! platform_get_os --
  !   Returns the operating system running on the current machine
  ! Arguments:
  !   No argument
  integer function platform_get_os ( )
    platform_get_os = PLATFORM_OS
  end function platform_get_os
  !
  ! platform_osstring --
  !   Returns a string containing the current operating system running on the current machine
  ! Arguments:
  !   currentos, output : the current operating system string
  subroutine platform_osstring ( currentos )
    character (len=*) , intent(out) :: currentos
    select case ( PLATFORM_OS )
    case ( PLATFORM_OS_WINDOWS_95 )
       currentos = "Windows 95"
    case ( PLATFORM_OS_WINDOWS_NT )
       currentos = "Windows NT"
    case ( PLATFORM_OS_MACOS )
       currentos = "MacOS"
    case ( PLATFORM_OS_SUNOS )
       currentos = "SunOS"
    case ( PLATFORM_OS_LINUX )
       currentos = "Linux"
    case ( PLATFORM_OS_UNIX )
       currentos = "Unix"
    case default
       call platform_error ( "platform_osstring" , "Unknown operating system." )
    end select
  end subroutine platform_osstring
  !
  ! platform_get_platform --
  !   Returns the general operating system running on the current machine
  ! Arguments:
  !   No argument
  integer function platform_get_platform ( )
    platform_get_platform = PLATFORM_PLATFORM
  end function platform_get_platform
  !
  ! platform_platformstring --
  !   Returns a string containing the current platform running on the current machine
  ! Arguments:
  !   currentplatform, output : the current platform string
  subroutine platform_platformstring ( currentplatform )
    character (len=*) , intent(out) :: currentplatform
    select case ( PLATFORM_PLATFORM )
    case ( PLATFORM_PLATFORM_WINDOWS )
       currentplatform = "Windows"
    case ( PLATFORM_PLATFORM_MAC )
       currentplatform = "Mac"
    case ( PLATFORM_PLATFORM_UNIX )
       currentplatform = "Unix"
    case default
       call platform_error ( "platform_platformstring" , "Unknown platform." )
    end select
  end subroutine platform_platformstring
  !
  ! platform_get_environment_variable --
  !   Returns the operating system running on the current machine
  ! Arguments:
  !   No argument
  subroutine platform_get_environment_variable ( envvar , value )
    character (len=*), intent(in) :: envvar
    character (len=*), intent(out) :: value
#ifdef _PLATFORM_INTEL_FORTRAN_PORTABILITY_ROUTINES
    integer :: environment_variable_length
#endif
#ifdef _PLATFORM_FORTRAN_2003
    call get_environment_variable ( envvar , value )
#endif
#ifdef _PLATFORM_INTEL_FORTRAN_PORTABILITY_ROUTINES
    environment_variable_length =  GETENVQQ ( envvar , value )
#endif
  end subroutine platform_get_environment_variable
  !
  ! platform_cd --
  !   Change working directory
  ! Arguments:
  !   filename   Name of the directory in which to enter
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !      upon return
  !
  subroutine platform_cd ( dirname , status )
    character(len=*), intent(in)             :: dirname
    integer, intent(out) , optional :: status
    integer :: local_status
#ifdef _PLATFORM_CHDIR_SUBROUTINE
    call chdir ( dirname , local_status )
#endif
#ifdef _PLATFORM_CHDIR_FUNCTION
    local_status = chdir ( dirname )
#endif
    if (present ( status )) then
       status = local_status
    endif
  end subroutine platform_cd
  !
  ! platform_stat --
  !   Get status of a file.
  ! Arguments:
  !   filename   Name of the file
  !   statarray  Array of size 13,
  !       statarray(1) Device ID
  !       statarray(2) Inode number
  !       statarray(3) File mode
  !       statarray(4) Number of links
  !       statarray(5) Owner’s uid
  !       statarray(6) Owner’s gid
  !       statarray(7) ID of device containing directory entry for file (0 if not available)
  !       statarray(8) File size (bytes)
  !       statarray(9) Last access time
  !       statarray(10) Last modification time
  !       statarray(11) Last file status change time
  !       statarray(12) Preferred I/O block size (-1 if not available)
  !       statarray(13) Number of blocks allocated (-1 if not available)
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !      upon return
  !
  subroutine platform_stat ( filename , statarray , status )
    character(len=*), intent(in)             :: filename
    integer, dimension (1:13) , intent(out)  :: statarray
    integer, intent(out) , optional :: status
    integer  :: local_status
#ifdef _PLATFORM_STAT_SUBROUTINE
    call stat ( filename , statarray , local_status )
#endif
#ifdef _PLATFORM_STAT_FUNCTION
    local_status = stat ( filename , statarray )
#endif
    if (present ( status )) then
       status = local_status
    endif
  end subroutine platform_stat
  !
  ! platform_error --
  !   Manage an error for the filedir module
  ! Arguments :
  !   origin : the name of the subroutine/function which generated the error.
  !   message : the message to display
  !
  subroutine platform_error ( origin , message )
    character(len=*), intent(in) :: origin
    character(len=*), intent(in) :: message
    write(6,*) "Error in m_platform."
    write(6,*) "Origin: ", trim(origin)
    write(6,*) "Error: ", trim(adjustl(message))
    stop
  end subroutine platform_error
end module m_platform

