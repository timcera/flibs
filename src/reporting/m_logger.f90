! m_logger.f90 --
!
!     This module allows the user to manage a log file, which is an execution
!     report of the program.
!     The client has to use the following subroutines in that order :
!     call log_init (log_file, timestamp) : initialises the logging associated
!              with the given filename.
!     call log_msg (msg)       : writes the message on the standard output
!              channel and onto the file
!     call log_shutdown ()     : shutdown the logging (closes the output channel
!              associated with the file.
!     call log_delimiter(type) : add a separating string of some level
!
!     Author: Michael Baudin
!     Changes: Arjen Markus
!
!     $Id$
!
module m_logger
    implicit none
    private

    public :: log_msg
    public :: log_init
    public :: log_shutdown
    public :: get_log_unit
    public :: log_delimiter
    public :: log_get_delimiter
    public :: log_inactivate_file
    public :: log_activate_file
    public :: log_inactivate_screen
    public :: log_activate_screen

    ! File unit associated with the log file
    integer :: log_unit = 6
    ! Logical set to false if the user wants to inactivate
    ! the logger ouput to screen
    ! The default value is true (logger activated).
    logical :: activate_screen = .true.
    ! Set to true to activate the logging into the file
    logical :: activate_file = .true.
    ! Set to true to include a timestamp
    logical :: log_time = .false.
    ! List of available delimiters levels
    integer , parameter , public :: LOG_LEVEL_VOLUME = 1
    integer , parameter , public :: LOG_LEVEL_CHAPTER = 2
    integer , parameter , public :: LOG_LEVEL_SECTION = 3
    integer , parameter , public :: LOG_LEVEL_SUBSECTION = 4

contains

! log_init --
!     Initialises the logging management by opening the given file on the
!     file unit log_unit.
!
! Arguments:
!     log_file           Name of the log file
!     timestamp          Add a timestamp or not (default: no)
!
subroutine log_init (log_file, timestamp)
    character(len=*), intent(in) :: log_file
    logical, intent(in), optional :: timestamp

    log_unit = log_get_unit()
    open( log_unit, FILE= log_file , ACTION='WRITE', STATUS='UNKNOWN', &
        POSITION ='APPEND')

    log_time = .false.
    if ( present(timestamp) ) then
        log_time = timestamp
    endif
end subroutine log_init

! log_shutdown --
!     Shutdown the logging management by closing the unit file
!     associated with log_unit.
!
! Arguments:
!     None
!
subroutine log_shutdown ()
    close ( log_unit )
end subroutine log_shutdown

! log_msg --
!     Writes the given message string "msg" on the standard output
!     and on the log file.
!
! Arguments:
!     msg           Log message to be written
!
! Note :
!    Before outputting directly the message string, the string is
!    trimmed, that is to say that all trailing blanks are removed from
!    the string.
!
subroutine log_msg( msg )
    character(len=*), intent(in) :: msg
    character(len=40)            :: date_string
    character(len=40)            :: time_string
    character(len=40)            :: stamp

    if ( log_time ) then
        call date_and_time( date = date_string, time = time_string )
        write( stamp, '(11a)' ) &
            date_string(1:4), '-', date_string(5:6), '-', date_string(7:8), ' ',&
            time_string(1:2), ':', time_string(3:4), ':', time_string(5:6)
    else
        stamp = ' '
    endif

    if (activate_screen) then
        if ( log_time ) then
            call log_write ( -1, trim(stamp) // ' ' // msg )
        else
            call log_write ( -1, msg )
        endif
    endif
    if (activate_file) then
        if ( log_time ) then
            call log_write ( log_unit, trim(stamp) // ' ' // msg )
        else
            call log_write ( log_unit, msg )
        endif
    endif
end subroutine log_msg

! log_write --
!     Write the given log message "msg" of length "length"
!     on the unit "unit".
!     Trim the given string before writing the string.
!
! Arguments:
!     unit             LU-number to write to (-1 is the screen)
!     msg              Message
!
subroutine log_write( unit, msg )
    integer, intent(in) :: unit
    character(len=*), intent(in) :: msg

    character(len=500)           :: filename

    if (  unit == -1 ) then
        write ( *, '(a)' ) trim(msg)
    else
        write ( unit, '(a)' ) trim(msg)

        !
        ! Flush the file
        !
        inquire( unit, name = filename )
        close( unit )
        open( unit, FILE=filename, ACTION='WRITE', STATUS='UNKNOWN', &
            POSITION ='APPEND')
    endif
end subroutine log_write

! get_log_unit --
!     Returns the unit number used in the logger
!
! Arguments:
!     None
!
integer function get_log_unit ()

    get_log_unit = log_unit

end function get_log_unit

! log_delimiter --
!     Log a delimiter of given level.
!
! Arguments:
!     level            Level to be written
!
subroutine log_delimiter( level )
    integer , intent(in), optional :: level
    character(len=40)              :: msg
    integer                        :: used_level
    if (present(level)) then
       used_level = level
    else
       used_level = 1
    endif
    call log_get_delimiter( used_level , msg )
    call log_msg( msg )
end subroutine log_delimiter

! log_get_delimiter --
!     Returns a log delimiter of given level.
!
! Arguments:
!     level             Level in question
!     msg               Corresponding string
!
 subroutine log_get_delimiter ( level , msg )
    implicit none
    integer , intent(in) :: level
    character(len=*), intent(out) :: msg
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
       write(*,*) "Bad value for the message level:" , level
       write(*,*)
       stop
    end select
end subroutine log_get_delimiter

! log_inactivate_file, ...
!     Control the output
!
!
! Allows to inactivate the logging into the file.
!
subroutine log_inactivate_file ()

    activate_file = .false.

end subroutine log_inactivate_file
!
! Allows to activate the logging into the file.
!
subroutine log_activate_file ()

    activate_file = .true.

end subroutine log_activate_file
!
! Allows to inactivate the logging on screen.
!
subroutine log_inactivate_screen ()

    activate_screen = .false.

end subroutine log_inactivate_screen
!
! Allows to activate the logging on screen.
!
subroutine log_activate_screen ()

    activate_screen = .true.

end subroutine log_activate_screen

integer function log_get_unit ( )

    integer :: iunit
    integer :: ios
    logical :: lopen
    logical :: unit_found
    iunit = 0
    unit_found = .false.
    log_get_unit = 0
    do iunit = 1, 100
        if ( iunit /= 5 .and. iunit /= 6 .and. iunit /= 9 ) then
            inquire ( UNIT = iunit, opened = lopen, iostat = ios )
            if ( ios == 0 ) then
                if ( .not. lopen ) then
                    log_get_unit = iunit
                    unit_found = .true.
                    exit
                endif
            endif
        endif
    enddo
    if (.NOT.unit_found) then
       write(*,*) "Logging: No free logical unit for log file"
    endif
end function log_get_unit

end module m_logger
