! messages.f90 --
!     Module (main part of a module) to handle messages
!     passed between steps in a computation
!     The idea is that this makes certain types of programming
!     easier, as the steps can be implemented independent
!     of the logic that selects them.
!
!     Requirements:
!     - This file is included in a module - see example/test program
!     - A module "msg_data_definitions" that defines:
!       -  Type STATE_DATA
!       -  Type MSG_DATA
!
type MESSAGE
    character*len=40) :: name
    integer           :: time
    type(MSG_DATA)    :: data
end type

private :: MESSAGE
type(MESSAGE), dimension(:), allocatable, private :: queue
integer, save, private                            :: max_queue = 1000

...

public  :: msg_loop, msg_put, msg_put_back
private :: msg_loop_print
private :: msg_loop_default
private :: msg_print_default

interface msg_loop
    module procedure msg_loop_print
    module procedure msg_loop_default
end interface

contains

! msg_exit --
!     Sets the system time to signal the end of the computation
!
! Arguments:
!     None
!
subroutine msg_exit
    system_time = stop_time
end subroutine msg_exit

! msg_put_back --
!     Tells the message loop to keep the current message
!
! Arguments:
!     None
!
subroutine msg_put_back
    delete = .false.
end subroutine msg_put_back

! msg_delete_current --
!     Tells the scanning message loop to delete the current message
!
! Arguments:
!     None
!
subroutine msg_delete_current
    delete_current = .true.
end subroutine msg_delete_current

! msg_system_time --
!     Return the current system time
!
! Arguments:
!     None
!
subroutine msg_system_time( time )
    integer, intent(out) :: time

    time = system_time

end subroutine msg_system_time

! msg_loop_default --
!     Dispatch the messages to the routine that actually handles them.
!     Default debug printing facilities used.
!
! Arguments:
!     state         The state of the computation
!     machine       The routine that handles the messages
!
subroutine msg_loop_default( state, machine )
    type(STATE_DATA) :: state
    interface
        subroutine machine( state, msg, data )
            use msg_data_definitions
            implicit none
            type(STATE_DATA), intent(inout) :: state
            character(len=*), intent(in)    :: msg
            integer, intent(in)             :: time
            type(MSG_DATA), intent(in)      :: data
        end subroutine
    end interface

    call msg_loop_print( state, machine, msg_print_default )

end subroutine msg_loop_default

! msg_loop_print --
!     Dispatch the messages to the routine that actually handles them.
!     Use specific debug printing facilities.
!
! Arguments:
!     state         The state of the computation
!     machine       The routine that handles the messages
!     print_debug   The routine that prints debug information
!
subroutine msg_loop_print( state, machine, print_debug )
    type(STATE_DATA) :: state
    interface
        subroutine machine( state, msg, time, data )
            use msg_data_definitions
            implicit none
            type(STATE_DATA), intent(inout) :: state
            character(len=*), intent(in)    :: msg
            integer, intent(in)             :: time
            type(MSG_DATA), intent(in)      :: data
        end subroutine
    end interface
    interface
        subroutine print_debug( lurep, state, msg, time, data )
            use msg_data_definitions
            implicit none
            integer, intent(in)             :: lurep
            type(STATE_DATA), intent(inout) :: state
            character(len=*), intent(in)    :: msg
            integer, intent(in)             :: time
            type(MSG_DATA), intent(in)      :: data
        end subroutine
    end interface

!
! This _uninitialised_ variable is used for special messages
!
    type(MSG_DATA) :: neutral_data

!
! Initialise the queue
!
    first_msg = 1
    last_msg  = 0
    call msg_put( 'INIT', start_time, data )

!
! Enter the message loop
!
    stop        = .false.
    system_time = start_time

    do while( system_time < stop_time )
        do i = first_msg, last_msg
            if ( queue(i)%time >= 0 ) then
                idmsg = i
                exit
            endif
        enddo

        delete = .true.

        !
        ! Pass the message to the handler,
        ! print debug information, if requested
        !
        if ( debug ) then
            call print_debug( lurep, state, queue(idmsg)%name, &
                     queue(idmsg)%time, queue(idmsg)%data )
        endif

        call machine( state, queue(idmsg)%name, queue(idmsg)%time, &
                 queue(idmsg)%data )

        !
        ! A message that has been passed, will be
        ! deleted, unless msg_put_back() has been called.
        !
        if ( delete ) then
            queue(idmsg)%time = -1
        endif

        !
        ! Always increase the ID of the first message to
        ! be examined
        !
        first_msg = first_msg + 1

        !
        ! Put in a 'TIME' message, if we have handled
        ! all messages with a creation time smaller than
        ! the current system time
        !
        if ( queue(first_msg)%time > system_time ) then
            system_time = system_time + 1
            call msg_put( 'TIME', system_time, neutral_data )
        endif
    enddo

end subroutine msg_loop_print

! msg_put --
!     Put a message in the (sorted) message queue
!
! Arguments:
!     msg           Name/type of the message
!     time          Time at which the message is posted
!     data          Data associated with the message
!
subroutine msg_put( msg, time, data )
    character(len=*), intent(in) :: msg
    integer, intent(in)          :: time
    type(MSG_DATA), intent(in)   :: data

    !
    ! Do we have enough space for the message in the queue"?
    !
    if ( last_msg >= max_queue ) then
        mask     = queue%time /= -1
        last_msg = count(mask)
        if ( last_msg >= max_queue ) then
            error = .true.
            call msg_error( "Queue full" )
            return
        else
            queue(1:last_msg) = pack( queue, mask )
        endif
    endif

    !
    ! Insert the message in the queue, first the
    ! special times
    !

    later_time = system_time + 1
    if ( last_msg > 0 ) later_time = queue(last_msg)%time + 1

    if ( time == msg_last ) then
        last_msg             = last_msg + 1
        queue(last_msg)%name = msg
        queue(last_msg)%time = later_time
        queue(last_msg)%data = data
        return
    endif

    insert = last_msg + 1

    if ( msg == msg_now ) then
        do i = 1,last_msg
            if ( queue(i)%time > system_time ) then
                insert = i ! Insert before this message
                exit
            endif
        enddo
    elseif ( time == msg_immediate ) then
        do i = 1,last_msg
            if ( queue(i)%time >= system_time ) then
                insert = i ! Insert before this message
                exit
            endif
        enddo
    else
        do i = 1,last_msg
            if ( queue(i)%time > time ) then
                insert = i ! Insert before this message
                exit
            endif
        enddo
    endif

    queue(insert+1:last_msg+1) = queue(insert:last_msg)
    queue(insert)%name         = msg
    queue(insert)%time         = time
    queue(insert)%data         = data
    last_msg                   = last_msg + 1
end subroutine msg_put

! End of general module text
