!
! This module allows the user to manage a log file, which is an execution
! report of the program.
! The client has to use the following subroutines in that order :
! call log_init (log_file) : initialises the logging associated 
!          with the given filename.
! call log_msg (msg)       : writes the message on the standard output 
!          channel and onto the file
! call log_shutdown ()     : shutodown the logging (closes the output channel 
!          associated with the file.
!
module m_logger
  use m_filesystem, only : file_get_unit
  use m_filesystem_parameters, only : MAX_FILENAME
  implicit none 
  private
  public :: log_msg
  public :: log_LENMSG
  public :: log_init
  public :: log_shutdown
  public :: get_log_unit
  public :: log_delimiter
  public :: log_get_delimiter
  public :: log_inactivate_file
  public :: log_activate_file
  public :: log_inactivate_screen
  public :: log_activate_screen  
  ! Maximal length of the log messages
  integer, parameter :: log_LENMSG = 500
  ! File unit associated with the log file
  integer :: log_unit = 6
  ! Logical set to false if the user wants to inactivate
  ! the logger ouput to screen
  ! The default value is true (logger activated).
  logical :: activate_screen = .true.
  ! Set to true to activate the logging into the file
  logical :: activate_file = .true.
  ! List of available delimiters levels
  integer , parameter , public :: LOG_LEVEL_VOLUME = 1
  integer , parameter , public :: LOG_LEVEL_CHAPTER = 2
  integer , parameter , public :: LOG_LEVEL_SECTION = 3
  integer , parameter , public :: LOG_LEVEL_SUBSECTION = 4
contains
  !
  ! Initialises the logging management by opening the given file on the 
  ! file unit log_unit.
  !
  subroutine log_init (log_file)
    implicit none
    character(len=MAX_FILENAME), intent(in) :: log_file
    call file_get_unit(log_unit)
    open (log_unit, FILE= log_file , ACTION='WRITE', STATUS='UNKNOWN')
  end subroutine log_init
  !
  ! Shutdown the logging management by closing the unit file associated with log_unit.
  !
  subroutine log_shutdown ()
    implicit none
    close ( log_unit )
  end subroutine log_shutdown
  !
  ! Writes the given message string "msg" on the standard output 
  ! and on the log file.
  ! Note :
  ! Before outputing directly the message string, the string is
  ! trimmed, that is to say that all trailing blanks are removed from the
  ! string. Therefore, the resulting output string is of length smaller or 
  ! equal to log_LENMSG.
  ! 
  subroutine log_msg (msg)
    implicit none
    character(len=log_LENMSG), intent(in) :: msg
    if (activate_screen) then
       call log_write ( 6 , log_LENMSG , msg )
    endif
    if (activate_file) then
       call log_write ( log_unit , log_LENMSG , msg )
    endif
  end subroutine log_msg
  !
  ! Write the given log message "msg" of length "length"
  ! on the unit "unit".
  ! Trim the given string before writing the string.
  !
  subroutine log_write ( unit , length , msg )
    implicit none
    integer, intent(in) :: unit, length
    character(len=length), intent(in) :: msg
    ! The intrinsic function "len_trim(msg)" should have the 
    ! same behaviour as "len(trim(msg))", but it has not !!!
    ! If I use "len_trim", the "trimmed" string as an "undefined" address in the
    ! debugger and the resulting string contains unprintable characters.
    ! If I use "len(trim(msg))", it works fine, so I keep it.
    character (len=len(trim(adjustl(msg)))) :: trimmed
    trimmed = trim(adjustl(msg))
    write ( unit , * ) trimmed 
  end subroutine log_write
  !
  ! Returns the unit number used in the logger
  !
  integer function get_log_unit ()
    implicit none
    get_log_unit = log_unit
  end function get_log_unit
  !
  ! Log a delimiter of given level.
  ! 
  subroutine log_delimiter (level)
    implicit none
    integer , intent(in), optional :: level
    character(len=log_LENMSG) :: msg
    integer :: used_level
    if (present(level)) then
       used_level = level
    else
       used_level = 1
    endif
    call log_get_delimiter ( used_level , msg )
    call log_msg (msg)
  end subroutine log_delimiter
  !
  ! Returns a log delimiter of given level.
  ! 
  subroutine log_get_delimiter ( level , msg )
    implicit none
    integer , intent(in) :: level
    character(len=log_LENMSG), intent(out) :: msg
    select case (level)
    case (LOG_LEVEL_VOLUME)
       write(msg,*) "==============="
    case (LOG_LEVEL_CHAPTER)
       write(msg,*) "---------------"
    case (LOG_LEVEL_SECTION)
       write(msg,*) "***************"
    case (LOG_LEVEL_SUBSECTION)
       write(msg,*) "+++++++++++++++"
    case default
       ! NOTE :
       ! We do not use m_exception here to limit the dependencies of
       ! such a low level utility.
       write(msg,*) "Bad value for the message level:" , level
       write(msg,*)
       stop
    end select
  end subroutine log_get_delimiter
  !
  ! Allows to inactivate the logging into the file.
  !
  subroutine log_inactivate_file ()
    implicit none
    activate_file = .false.
  end subroutine log_inactivate_file
  !
  ! Allows to activate the logging into the file.
  !
  subroutine log_activate_file ()
    implicit none
    activate_file = .true.
  end subroutine log_activate_file
  !
  ! Allows to inactivate the logging on screen.
  !
  subroutine log_inactivate_screen ()
    implicit none
    activate_screen = .false.
  end subroutine log_inactivate_screen
  !
  ! Allows to activate the logging on screen.
  !
  subroutine log_activate_screen ()
    implicit none
    activate_screen = .true.
  end subroutine log_activate_screen  
end module m_logger

