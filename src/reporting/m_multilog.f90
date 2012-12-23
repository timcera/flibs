! m_multilog.f90
!
!   The module m_multilog provides a logging type with a set of type
!   bound procedures and other methods to manage one or more log
!   files. The module and its test program are based on the m_logger
!   module (including unit test) which is part of the flibs library.
!   For applications in need of only one log, m_logger is the
!   recommended module.
!
! Overview
!
!   The goal of this component is to provide a way to write messages
!   both on standard output and on one or more log files, so that a
!   trace of the execution - with desired level of detail - can be
!   read by the user after the execution.
!
!   One single statement should be able to write to all open log files
!   with appropriate log level set. Example:
!
!      call log_msg ('Log message', WARNING)
!
!   will write to logs with log level equal to or lower than
!   WARNING, i.e. WARNING, INFO and FINE.
!
!   The module provides methods to
!   - connect a file to a log object
!   - configure the logging process, for example disable the standard
!     output messages or enable time stamps,
!   - log messages,
!   - group log objects,
!   - write log messages to a group of log objects.
!
!   Individual logs are started with log_t%startup (filename [,options]).
!   The method takes the log file name as first argument :
!   its main purpose is to connect the log to the file.
!   Optional arguments: log information level, append true/false
!
!   Logs can be added to the log group with "add_log". Logs can be
!   removed from the log group using the "remove_log" method, which will
!   also close the file connected to the log.
!
!   The messages are sent to one or more of the active logs with the
!   static method "log_msg". Infolevel is an optional argument to
!   log_msg, if left out it's set to ALL which means that the message
!   will be written to all logs.
!
!   In the following example, extracted from the unit tests of
!   m_multilog provided with the project, one connects the file
!   "test_m_multilog.log" to the log, two messages of which only the
!   first should be written to file and shut down the logging system.
!
!      type (log_t) :: test
!
!      call test%startup ( 'test_m_multilog.log' , INFO )
!      call add_log ( test )
!      call log_msg ( 'First message' , INFO )
!      call log_msg ( 'Second message', FINE )
!      call shutdown_log_group ()
!
!    By default, the logging is done on all log files with appropriate
!    infolevel and on standard output. The user may want to configure
!    the behaviour of the log_group so that message are not written on
!    standard output.
!
!    The static method "log_configure(option,value [,log])" is the central
!    point to configure the log group. It takes a character "option"
!    string and a "value" as arguments. In the following example, one
!    writes messages on file and in some cases also to stdout, with
!    and without time stamps.
!
!      type (log_t) :: test
!
!      call test%startup ( 'test_m_multilog.log' )
!      call add_log ( test )
!      call log_configure ( "writeonstdout" , .false. )
!      call log_msg( 'This message is written only on file' )
!      call log_configure ( "writeonstdout" , .true. )
!      call log_msg( 'This message is written both on screen and on file' )
!      call shutdown_log_group ()
!
! TODO   The order of the info levels - should it be different?
!        Possibility to define own info levels and/or re-configure
!        the standard ones?
!
! Author: Karin Nyström, 2012, knystrom at users.sourceforge.net
!
module m_multilog
  use iso_fortran_env
  implicit none
  private
  public :: shutdown_log_group
  public :: add_log
  public :: remove_log
  public :: log_cget
  public :: log_configure
  public :: log_delimiter
  public :: log_msg
  public :: log_reset
  !
  ! Interfaces for procedures using the global log_group object. Convert
  ! to type bound if there is need to use more than one simultaneous
  ! log group. (One log group handles several log files.)
  !
  interface log_configure
     module procedure configure_logical
     module procedure configure_integer
     module procedure configure_character
  end interface log_configure
  interface log_cget
     module procedure cget_logical
     module procedure cget_integer
     module procedure cget_character
  end interface log_cget
  !
  ! Log type - one single log
  !
  type, public :: log_t
     character(len=500) :: filename
     integer :: logindex
     integer :: fileunit
     integer :: infolevel
     ! Set to true to include a timestamp
     logical :: timestamp
     ! Logical set to false if the user wants to inactivate the
     ! ouput to screen. The default value is true.
     logical :: writestdout
   contains
     procedure :: startup
     procedure :: shutdown
     procedure :: write
  end type log_t
  !
  ! Log group type - handles one to four logs
  !
  type :: log_group_t
     ! Keep track of current number of logs connected to this logger
     integer :: nroflogs
     ! Try to abort program when error in logger occurs?
     logical :: stoponerror
     ! Four info_lvl values -> array of max four log files
     type (log_t) :: log_array(4)
  end type log_group_t
  !
  ! Static fields
  !
  ! Log handler - global. Could remove this and change procedures
  ! related to logger to type bound if there is need for more than one
  ! log handler.
  !
  type (log_group_t), public :: log_group
  ! Logical unit associated with the standard output
  integer :: log_stdout = OUTPUT_UNIT
  !
  ! Strings used as default delimiters
  !
  integer , parameter , public :: LOG_LEVEL_DELIMITER_LENGTH = 50
  character (len=LOG_LEVEL_DELIMITER_LENGTH) :: log_level_string_volume = "==============="
  character (len=LOG_LEVEL_DELIMITER_LENGTH) :: log_level_string_chapter = "---------------"
  character (len=LOG_LEVEL_DELIMITER_LENGTH) :: log_level_string_section = "***************"
  character (len=LOG_LEVEL_DELIMITER_LENGTH) :: log_level_string_subsection = "+++++++++++++++"
  !
  ! List of available delimiters levels
  !
  integer , parameter , public :: LOG_LEVEL_VOLUME = 1
  integer , parameter , public :: LOG_LEVEL_CHAPTER = 2
  integer , parameter , public :: LOG_LEVEL_SECTION = 3
  integer , parameter , public :: LOG_LEVEL_SUBSECTION = 4
  !
  ! List of available log information levels
  ! DEBUG   - all messages
  ! FINE    - all messages
  ! INFO    - error, warning and info
  ! WARNING - error and warning
  ! ERROR   - only error messages
  !
  integer , parameter , public :: DEBUG   = 0
  integer , parameter , public :: FINE    = 0
  integer , parameter , public :: INFO    = 1
  integer , parameter , public :: WARNING = 2
  integer , parameter , public :: ERROR   = 3
  integer , parameter , public :: ALL     = 9

contains
  !
  ! Type bound procedures - log_t
  !
  ! startup --
  !     Initialises the log, connect it to the given filename
  !     and set default values.
  !
  ! Arguments:
  !     log_file           Name of the log file
  !     append, optional :
  !      - if present and true, then the messages will be appended
  !        to the end of the log file.
  !      - if present and false, then the initialization of the
  !        log overwrites the messages of the previous logging session.
  !      - if not provided, the default value is append=.true.
  !     info_lvl, optional:
  !      - if present, set log_t%infolevel to the provided value
  !      - if not present, set to default value INFO
  subroutine startup ( this, log_file, append, info_lvl )
    class(log_t),        intent(in out) :: this
    character(len=*),    intent(in)     :: log_file
    logical, intent(in), optional       :: append
    integer, intent(in), optional       :: info_lvl
    logical :: append_real
    !
    ! Process options
    !
    if ( present ( append ) ) then
       append_real = append
    else
       append_real = .true.
    end if
    if ( append_real ) then
       open (NEWUNIT=this%fileunit, FILE=log_file , ACTION='WRITE', STATUS='UNKNOWN', &
            POSITION ='APPEND')
    else
       open (NEWUNIT=this%fileunit, FILE=log_file , ACTION='WRITE', STATUS='UNKNOWN')
    end if
    ! Set file name
    this%filename = log_file
    ! Default: no output to stdout
    this%writestdout = .false.
    ! Set log info level to provided value or default
    if ( present ( info_lvl ) ) then
       this%infolevel = info_lvl
    else
       this%infolevel = INFO
    end if
    this%logindex = 0
  end subroutine startup
  ! shutdown --
  !     Shutdown the log
  !
  ! Arguments:
  !     None
  !
  subroutine shutdown ( this )
    class(log_t), intent(in out) :: this
    close ( this%fileunit )
  end subroutine shutdown
  ! write --
  !   Log the given character string to one logging unit.
  !   If the logging to standard output is enabled, writes the message
  !   on standard output.
  !   If the logging to the log file is enabled, writes the message
  !   into the log file.
  !   Before outputting directly the message string, the string is
  !   trimmed, that is to say that all trailing blanks are removed from
  !   the string.
  !   If the time stamp option is enabled, a time stamp with
  !   format "year-month-day hh:mm:ss" is inserted before the message.
  !
  ! Arguments:
  !     msg           Log message to be written
  !     info_lvl      Optional: log information level, default = INFO
  !
  ! Note :
  !    Before outputting directly the message string, the string is
  !    trimmed, that is to say that all trailing blanks are removed from
  !    the string.
  !
  subroutine write( this, msg, info_lvl )
    class(log_t),      intent(in out) :: this
    character(len=*),  intent(in)     :: msg
    integer, optional, intent(in)     :: info_lvl
    character(len=40) :: date_string
    character(len=40) :: time_string
    character(len=40) :: stamp
    if ( this%timestamp ) then
       call date_and_time( date = date_string, time = time_string )
       write( stamp, '(11a)' ) &
            date_string(1:4), '-', date_string(5:6), '-', date_string(7:8), ' ',&
            time_string(1:2), ':', time_string(3:4), ':', time_string(5:6)
    else
       stamp = ' '
    end if
    if ( this%writestdout ) then
       if ( this%timestamp ) then
          call log_write ( log_stdout, trim(stamp) // ' ' // msg, info_lvl )
       else
          call log_write ( log_stdout, msg, info_lvl )
       end if
    end if
    ! Always write to log file
    if ( this%timestamp ) then
       call log_write ( this%fileunit, trim(stamp) // ' ' // msg, info_lvl)
    else
       call log_write ( this%fileunit, msg, info_lvl )
    end if
  end subroutine write
  ! ------------------------------------------------------------
  !
  ! Procedures related to the global log handler 'log_group'
  !
  ! shutdown_log_group --
  !   Close all logs connected to the log_group object
  !
  subroutine shutdown_log_group ( )
    integer :: i
    do i = 1, log_group%nroflogs
       call log_group%log_array(i)%shutdown ()
    end do
    log_group%nroflogs = 0
  end subroutine shutdown_log_group
  ! add_log --
  !   Add a log_t object to the log_group
  !
  ! Arguments:
  !   log        log_t object (initialized with log_t%startup)
  !
  subroutine add_log ( log )
    type(log_t), intent(in out) :: log
    log_group%nroflogs = log_group%nroflogs + 1
    if ( log_group%nroflogs > size (log_group%log_array) ) then
       call log_error ('add_log: Tried to add log to a full log group.')
    else
       log_group%log_array(log_group%nroflogs) = log
    end if
    log%logindex = log_group%nroflogs
  end subroutine add_log
  ! remove_log --
  !   Remove a log_t object from the log_group and close the
  !   connected file.
  !
  ! Arguments:
  !   log        log_t object
  !
  subroutine remove_log ( log )
    type(log_t), intent(in out) :: log
    integer :: idx
    idx = log%logindex
    ! Move logs with higher index than the removed log down one step
    if ( idx < log_group%nroflogs ) then
       do idx = log%logindex, log_group%nroflogs - 1
          log_group%log_array(idx) = log_group%log_array(idx + 1)
       end do
    end if
    log_group%nroflogs = log_group%nroflogs - 1
    call log%shutdown ()
  end subroutine remove_log
  ! log_msg --
  !   Log the given character string to all logging units with relevant
  !   info_lvl. If no info_lvl is specified, ALL is used (write to all
  !   logs).
  !   If the logging to standard output is enabled, writes the message
  !   on standard output as well as to relevant log files.
  !   Before outputting directly the message string, the string is
  !   trimmed, that is to say that all trailing blanks are removed from
  !   the string.
  !   If the time stamp option is enabled, a time stamp with
  !   format "year-month-day hh:mm:ss" is inserted before the message.
  !
  ! Arguments:
  !     msg           Log message to be written
  !     info_lvl      Optional: log information level, default = INFO
  !
  subroutine log_msg ( msg, info_lvl )
     character(len=*),   intent(in)     :: msg
     integer, optional,  intent(in)     :: info_lvl
     integer :: i, used_level
     if ( present ( info_lvl ) ) then
        used_level = info_lvl
     else
        used_level = ALL
     end if
     if ( log_group%nroflogs < 1 ) then
        call log_error ('log_msg: Tried to write to empty log group')
     else
        do i = 1, log_group%nroflogs
           if ( log_group%log_array(i)%infolevel <= used_level ) then
              call log_group%log_array(i)%write( msg, used_level )
           end if
        end do
     end if
  end subroutine log_msg
  !
  ! log_reset --
  !    Set all internal settings to default values.
  !
  ! Arguments:
  !    log        log to reset, if not provided all logs are reset
  !
  subroutine log_reset ( log )
    type(log_t), optional, intent(in out) :: log
    integer :: i

    if ( present ( log ) ) then
       log%infolevel = INFO
       log%timestamp = .false.
       log%writestdout = .false.
    else
       do i = 1, log_group%nroflogs
          log_group%log_array(i)%infolevel = INFO
          log_group%log_array(i)%timestamp = .false.
          log_group%log_array(i)%writestdout = .false.
       end do
    end if
  end subroutine log_reset
  !
  ! configure_logical --
  !   Set the logical static "option" of the component to "val".
  !   The "option" may be one of the following.
  ! option = "timestamp"
  !   Disable or enable the insertion of time stamps.
  !   If the time stamp option is enabled, a time stamp with
  !   format "year-month-day hh:mm:ss" is inserted before the message.
  ! option = "writeonstdout"
  !   Disable or enable the writing on standard output.
  !
  ! Arguments:
  !    option        see above
  !    val            "    "
  !    log           log file, optional. If not provided "option" will
  !                  be set to "val" for all logs connected to log_group
  !
  subroutine configure_logical ( option, val, log )
    character ( len = * ), intent(in)     :: option
    logical,               intent(in)     :: val
    type(log_t), optional, intent(in out) :: log
    character ( len = 500 ) :: message
    integer :: i

    select case ( option )
    case ( "stoponerror" )
       log_group%stoponerror = val
    case ( "timestamp" )
       if ( present ( log ) ) then
          log%timestamp = val
       else
          do i = 1, log_group%nroflogs
             log_group%log_array(i)%timestamp = val
          end do
       end if
    case ( "writeonstdout" )
       if ( present ( log ) ) then
          log%writestdout = val
       else
          do i = 1, log_group%nroflogs
             log_group%log_array(i)%writestdout = val
          end do
       end if
    case default
       write (message,"(A,A,A,l5,A)") "Unknown option ", option, &
            " for value ", val, " in configure_logical"
       call log_error ( message )
    end select
  end subroutine configure_logical
  !
  ! log_configure_integer --
  !   Set the integer static "option" of the component to "val".
  !   The "option" may be one of the following.
  ! option = "infolevel"
  !   Set level of detail in the log. Available values: ERROR, WARNING, INFO, FINE
  !
  ! Arguments:
  !    option        see above
  !    val            "    "
  !    log           log file, optional. If not provided "option" will
  !                  be set to "val" for all logs connected to log_group
  !
  subroutine configure_integer ( option , val, log )
    character ( len = * ) , intent(in)     :: option
    integer,                intent(in)     :: val
    type(log_t), optional,  intent(in out) :: log
    character ( len = 500 ) :: message
    integer :: i
    select case ( option )
    case ( "infolevel" )
       if ( val >= 0 .and. val <= 3 ) then
          if ( present ( log ) ) then
             log%infolevel = val
          else
             do i = 1, log_group%nroflogs
                log_group%log_array(i)%infolevel = val
             end do
          end if
       else
          write (message,"(A,I5,A)") "Unknown value ", val, &
               " for option infolevel in log_configure_integer"
          call log_error ( message )
       end if
    case default
       write (message,"(A,A,A,I5,A)") "Unknown option ", option, &
            " for value ", val, " in log_configure_integer"
       call log_error ( message )
    end select
  end subroutine configure_integer
  !
  ! log_configure_character --
  !   Set the character static "option" of the component to "val".
  !   The "option" may be one of the following.
  ! option = "level_string_volume"
  !   Set the string used for volume delimiter.
  ! option = "level_string_chapter"
  !   Set the string used for chapter delimiter.
  ! option = "level_string_section"
  !   Set the string used for section delimiter.
  ! option = "level_string_subsection"
  !   Set the string used for subsection delimiter.
  !
  ! Log delimiters are global (so far) so the 'log' option is not
  ! very meaningful.
  !
  subroutine configure_character ( option , val, log )
    character ( len = * ) , intent(in)     :: option
    character ( len = * ) , intent(in)     :: val
    type(log_t), optional,  intent(in out) :: log        ! Not used - added for symmetry
    character ( len = 500 ) :: message
    select case ( option )
    case ( "level_string_volume" )
       log_level_string_volume = val
    case ( "level_string_chapter" )
       log_level_string_chapter = val
    case ( "level_string_section" )
       log_level_string_chapter = val
    case ( "level_string_subsection" )
       log_level_string_chapter = val
    case default
       write (message,"(A,A,A,A,A)") "Unknown option ", option, &
            " for value ", val, " in log_configure_character"
       call log_error ( message )
    end select
  end subroutine configure_character
  !
  ! log_cget_logical --
  !   Returns the value of the given logical option.
  !   The "option" may be one of the following.
  ! option = "timestamp"
  !   Current value of the option to enable / disable insertion of time stamps.
  ! option = "writeonstdout"
  !   Current value of the option to enable / disable writing on standard output.
  ! option = "writeonlogfile"
  !   Current value of the option to enable / disable writing on log file.
  ! option = "stoponerror"
  !   Current value of the option to enable / disable stopping when an error is met.
  !
  ! Arguments:
  !    log           log file, optional. If not provided, .true. will be returned
  !                  if "option" is set to true for one or more logs
  !    option        see above
  !    val            "    "
  !
  subroutine cget_logical ( log, option , val )
    character ( len = * ) , intent(in)  :: option
    logical,                intent(out) :: val
    type(log_t), optional,  intent(in)  :: log
    character ( len = 500 ) :: message
    logical :: values(4)
    integer :: i

    val = .false.
    values(:) = .false.
    select case ( option )
    case ( "timestamp" )
       if ( present ( log ) ) then
          val = log%timestamp
       else
          do i = 1, log_group%nroflogs
             values(i) = log_group%log_array(i)%timestamp
          end do
          if (any (values .eqv. .true.) ) val = .true.
       end if
    case ( "writeonstdout" )
       if ( present ( log ) ) then
          val = log%writestdout
       else
          do i = 1, log_group%nroflogs
             values(i) = log_group%log_array(i)%writestdout
          end do
          if (any (values .eqv. .true.) ) val = .true.
       end if
    case ( "stoponerror" )
       val = log_group%stoponerror
    case default
       write (message,"(A,l5,A)") "Unknown option ", option, &
            " in log_cget_logical"
       call log_error ( message )
    end select
  end subroutine cget_logical
  !
  ! log_cget_integer --
  !   Returns the value of the given integer option.
  !   The "option" may be one of the following.
  ! option = "infolevel"
  !   Minimum level for writing messages
  ! option = "logfileunit"
  !   Current logical unit connected to the log provided as argument
  !
  subroutine cget_integer ( log, option, val )
    type(log_t),            intent(in)  :: log
    character ( len = * ) , intent(in)  :: option
    integer,                intent(out) :: val
    character ( len = 500 ) :: message
    select case ( option )
    case ( "infolevel" )
       val = log%infolevel
    case ( "logfileunit" )
       val = log%fileunit
    case default
       write (message,"(A,I5,A)") "Unknown option ", option, &
            " in log_cget_integer"
       call log_error ( message )
    end select
  end subroutine cget_integer
  !
  ! log_cget_character --
  !   Returns the value of the given logical option.
  !   The "option" may be one of the following.
  ! option = "level_string_volume"
  !   Get the string used for volume delimiter.
  ! option = "level_string_chapter"
  !   Get the string used for chapter delimiter.
  ! option = "level_string_section"
  !   Get the string used for section delimiter.
  ! option = "level_string_subsection"
  !   Get the string used for subsection delimiter.
  !
  subroutine cget_character ( log , option , val )
    type(log_t), optional , intent(in)  :: log       ! Not used - added for symmetry
    character ( len = * ) , intent(in)  :: option
    character ( len = * ) , intent(out) :: val
    character ( len = 500 ) :: message
    select case ( option )
    case ( "level_string_volume" )
       val = LOG_LEVEL_STRING_VOLUME
    case ( "level_string_chapter" )
       val = LOG_LEVEL_STRING_CHAPTER
    case ( "level_string_section" )
       val = LOG_LEVEL_STRING_SECTION
    case ( "level_string_subsection" )
       val = LOG_LEVEL_STRING_SUBSECTION
    case default
       write (message,"(A,A,A)") "Unknown option ", option, &
            " in log_cget_character"
       call log_error ( message )
    end select
  end subroutine cget_character
  ! -----------------------------------------------------------------
  !
  ! Message and error handling routines (not type bound)
  !
  ! log_write --
  !     Write the given log message "msg" of length "length"
  !     on the unit "unit".
  !     Trim the given string before writing the string.
  !
  ! Arguments:
  !     unit             LU-number to write to
  !     msg              Message
  !     info_lvl         Optional; FINE, INFO, WARNING or ERROR
  !                      INFO is the default log information level,
  !                      which is also used if provided value of
  !                      info_lvl is not recognized.
  !
  subroutine log_write( unit, msg, info_lvl )
    integer,           intent(in out) :: unit
    character(len=*),  intent(in)     :: msg
    integer, optional, intent(in)     :: info_lvl

    if ( unit == log_stdout ) then
       write ( *, '(a)' ) trim(msg)
    else
       if ( present ( info_lvl )) then
          select case (info_lvl)
          case (FINE,INFO)
             write ( unit, '(a)' ) trim(msg)
          case (WARNING)
             write ( unit, '(a)' ) 'Warning: '//trim(msg)
          case (ERROR)
             write ( unit, '(a)' ) 'ERROR: '//trim(msg)
          case default
             write ( unit, '(a)' ) trim(msg)
          end select
       else
          write ( unit, '(a)' ) trim(msg)
       end if
       !
       ! Flush the file
       !
       flush ( unit )
    end if
  end subroutine log_write
  ! log_delimiter --
  !   Log a delimiter of given level, to make so that the log file
  !   contain different visual parts.
  !   Available values for level are : LOG_LEVEL_VOLUME,
  !   LOG_LEVEL_CHAPTER, LOG_LEVEL_SECTION, LOG_LEVEL_SUBSECTION.
  !   If level is not provided, the default value for level is LOG_LEVEL_VOLUME.
  !
  ! Arguments:
  !     level            Level to be written
  !
  subroutine log_delimiter( log, level )
    type (log_t), optional, intent(in out) :: log
    integer,      optional, intent(in)     :: level
    character(len=40) :: msg ! the delimiter string
    integer           :: used_level
    if (present(level)) then
       used_level = level
    else
       used_level = LOG_LEVEL_VOLUME
    endif
    call log_get_delimiter( used_level , msg )
    if ( present ( log ) ) then
       call log%write( msg )
    else
       call log_msg( msg )
    end if
  end subroutine log_delimiter
  !
  ! log_error --
  !   Generates an error for the log handling
  !
  subroutine log_error ( message )
    implicit none
    character (len=*), intent(in) :: message
    write ( 6, "(A)" ) "Error in m_multilog."
    write ( 6 , "(A)" ) message
    call log_error_stop ( )
  end subroutine log_error
  !
  ! log_error_stop --
  !   Stop the execution if desired and possible.
  !
  subroutine log_error_stop ( )
    if ( log_group%stoponerror ) then
       stop
    endif
  end subroutine log_error_stop
  !
  ! log_set_stoponerror --
  !
  subroutine log_set_stoponerror ( stoponerror )
    logical , intent(in) :: stoponerror
    log_group%stoponerror = stoponerror
  end subroutine log_set_stoponerror
  !
  ! log_get_delimiter --
  !   Fills msg with a log delimiter of given level.
  !   Available values for level are : LOG_LEVEL_VOLUME,
  !   LOG_LEVEL_CHAPTER, LOG_LEVEL_SECTION, LOG_LEVEL_SUBSECTION
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

end module m_multilog

