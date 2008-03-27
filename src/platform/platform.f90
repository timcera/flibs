!
! This module provides low-level services to define and acces to the system.
!
! Operating System dependency :
! It should be compiled by defining several preprocessing macros, to
! select from the various operating systems and compilers for which this module
! is made for.
! Choose your OS between one of these :
! _OS_WINDOWS_95 , _OS_WINDOWS_NT , _OS_MAC , _OS_SUN , _OS_LINUX , _OS_UNIX
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
! _INTEL_FORTRAN_PORTABILITY_ROUTINES , _FORTRAN_2003
!
module platform
#ifdef _INTEL_FORTRAN_PORTABILITY_ROUTINES
  use ifport
#endif
  implicit none
  private
  !
  ! The name of the operating system running on this machine, such as Windows 95,
  ! Windows NT, MacOS, or SunOS. On UNIX machines, this is the value returned by
  ! uname -s.
  !
  integer, parameter, public :: PLATFORM_OS_WINDOWS_95 = 1
  integer, parameter, public :: PLATFORM_OS_WINDOWS_NT = 2
  integer, parameter, public :: PLATFORM_OS_MACOS = 3
  integer, parameter, public :: PLATFORM_OS_SUNOS = 4
  integer, parameter, public :: PLATFORM_OS_LINUX = 5
  integer, parameter, public :: PLATFORM_OS_UNIX = 6
  integer, parameter, public :: PLATFORM_OS_NB = 6
#ifdef _OS_WINDOWS_95
  integer, parameter, public :: PLATFORM_OS = PLATFORM_OS_WINDOWS_95
#endif
#ifdef _OS_WINDOWS_NT
  integer, parameter, public :: PLATFORM_OS = PLATFORM_OS_WINDOWS_NT
#endif
#ifdef _OS_MAC
  integer, parameter, public :: PLATFORM_OS = PLATFORM_OS_MACOS
#endif
#ifdef _OS_SUN
  integer, parameter, public :: PLATFORM_OS = PLATFORM_OS_SUNOS
#endif
#ifdef _OS_LINUX
  integer, parameter, public :: PLATFORM_OS = PLATFORM_OS_LINUX
#endif
#ifdef _OS_UNIX
  integer, parameter, public :: PLATFORM_OS = PLATFORM_OS_UNIX
#endif
  !
  ! Either windows, macintosh, or unix. This identifies the general operating environment of the machine.
  !
  integer, parameter, public :: PLATFORM_PLATFORM_WINDOWS = 1
  integer, parameter, public :: PLATFORM_PLATFORM_MAC = 2
  integer, parameter, public :: PLATFORM_PLATFORM_UNIX = 3
  integer, parameter, public :: PLATFORM_PLATFORM_NB = 3
#ifdef _OS_WINDOWS_95
  integer, parameter, public :: PLATFORM_PLATFORM = PLATFORM_PLATFORM_WINDOWS
#endif
#ifdef _OS_WINDOWS_NT
  integer, parameter, public :: PLATFORM_PLATFORM = PLATFORM_PLATFORM_WINDOWS
#endif
#ifdef _OS_MAC
  integer, parameter, public :: PLATFORM_PLATFORM = PLATFORM_PLATFORM_MAC
#endif
#ifdef _OS_SUN
  integer, parameter, public :: PLATFORM_PLATFORM = PLATFORM_PLATFORM_UNIX
#endif
#ifdef _OS_LINUX
  integer, parameter, public :: PLATFORM_PLATFORM = PLATFORM_PLATFORM_UNIX
#endif
#ifdef _OS_UNIX
  integer, parameter, public :: PLATFORM_PLATFORM = PLATFORM_PLATFORM_UNIX
#endif

  !
  ! Public static methods
  !
  public :: platform_system
  public :: platform_get_os
  public :: platform_get_platform
  public :: platform_get_environment_variable

contains
  !
  ! platform_system  --
  !    This subroutine allows to pass commands to the system
  !    and returns the execution status.
  !    Provides an interface to the system fortran extension.
  ! Arguments:
  !    status, optional : if supplied, it contains 0 on success or nonzero error code
  !        upon return
  !    Under windows, status are unused and therefore allways set to 0.
  ! Caution !
  !    Under Windows the "call system" may generate the display of a console.
  ! Caution !
  !    The standard is that a status different from 0 is an error.
  !    In fact, under Windows, the value of the status has no signification.
  !    I got status 1 or 5228135 with a good copy result.
  !
  subroutine platform_system ( command , status )
    implicit none
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

  ! platform_get_os --
  !    Returns the operating system running on the current machine
  ! Arguments:
  !    No argument
  integer function platform_get_os ( )
    implicit none
    platform_get_os = PLATFORM_OS
  end function platform_get_os

  ! platform_get_platform --
  !    Returns the general operating system running on the current machine
  ! Arguments:
  !    No argument
  integer function platform_get_platform ( )
    implicit none
    platform_get_platform = PLATFORM_PLATFORM
  end function platform_get_platform

  ! platform_get_environment_variable --
  !    Returns the operating system running on the current machine
  ! Arguments:
  !    No argument
  subroutine platform_get_environment_variable ( envvar , value )
    implicit none
    character (len=*), intent(in) :: envvar
    character (len=*), intent(out) :: value
#ifdef _INTEL_FORTRAN_PORTABILITY_ROUTINES
    integer :: environment_variable_length
#endif
#ifdef _FORTRAN_2003
    call get_environment_variable ( envvar , value )
#endif
#ifdef _INTEL_FORTRAN_PORTABILITY_ROUTINES
    environment_variable_length =  GETENVQQ ( envvar , value )
#endif
  end subroutine platform_get_environment_variable
end module platform

