!
! This program is a testing component for the module platform
!
program test_platform
  use platform, only : &
       platform_get_os, &
       platform_get_platform, &
       PLATFORM_OS_WINDOWS_NT, &
       platform_system, &
       PLATFORM_PLATFORM_WINDOWS, &
       PLATFORM_PLATFORM_UNIX
  implicit none
  integer :: os
  integer :: platform

  integer, parameter :: MSG_LEN = 500
  character (len= MSG_LEN ) :: msg
  character (len= MSG_LEN ) :: command
  integer :: status
  !
  ! test #1 : operating system
  !
  call logmsg ( "Test #1 : get_os" )
  ! Just test that it works
  os = platform_get_os ()
  write ( msg ,*) "Current os:" , os
  call logmsg ( msg )
  !
  ! test #2 : general platform
  !
  call logmsg ( "Test #2 : get_platform" )
  ! Just test that it works
  platform = platform_get_platform ()
  write ( msg ,*) "Current platform:" , platform
  call logmsg ( msg )
  !
  ! Test #3 : execute a command
  !
  call logmsg ( "Test #3 : platform_system" )
  select case (platform)
  case (PLATFORM_PLATFORM_WINDOWS)
     write ( command , *) "dir"
  case (PLATFORM_PLATFORM_UNIX)
     write ( command , *) "ls"
  case default
     write(6,*) "Unknown command to execute for platform :", platform
  end select
  call platform_system ( command , status )

contains
  subroutine assert (test, message)
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: message
    character(len=50) :: origin
    integer, parameter :: MSG_LEN = 200
    character (len= MSG_LEN ) :: msg
    origin = "test_filedir.f90"
    if (.NOT.test) then
       write(msg,*) "Internal error from: ", trim(origin)
       call logmsg ( msg )
       write(msg,*) "Error: ", trim(message)
       call logmsg ( msg )
       STOP
    else
       write(msg,*) "-> Test PASS"
       call logmsg ( msg )       
    endif
  end subroutine assert
  subroutine logmsg ( message )
    implicit none
    character(len=*), intent(in) :: message
    write(6,*) trim(message)
  end subroutine logmsg
end program test_platform


