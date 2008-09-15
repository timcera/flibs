! annealing.f90 --
!     Module (or rather the contents) for a simple implementation of
!     simulated annealing
!
!     $Id$
!

integer, parameter :: annealing_value  = 1
integer, parameter :: annealing_report = 2
integer, parameter :: annealing_done   = 3

type ANNEALING_PARAMETERS
    real    :: initial_temp
    real    :: temperature
    real    :: temp_reduction
    real    :: scale_factor
    integer :: number_iterations
    integer :: iteration_count
    integer :: state               = 0 ! Initialisation
    integer :: accepted
    logical :: verbose
    logical :: automatic_scaling
    logical :: changes
endtype ANNEALING_PARAMETERS

contains

! set_parameters --
!     Initialise or update the annealing parameters
!
! Arguments:
!     params              The structure holding the parameters
!     update              If true, only update the parameters in the argument list
!                         otherwise re-initialise
!     initial_temp        Initial temperature
!     temp_reduction      Factor by which to reduce the temperature
!     number_iterations   Number of iterations before reducing the temperature
!     scale_factor        Factor by which to scale the values
!     automatic_scaling   If true, first determine a useful scale factor
!     verbose             If true, print
!
subroutine set_parameters( params, update, initial_temp, temp_reduction, &
                           number_iterations, scale_factor, automatic_scaling, verbose )
    type(ANNEALING_PARAMETERS), intent(inout) :: params
    logical, intent(in)                       :: update
    real, intent(in), optional                :: initial_temp
    real, intent(in), optional                :: temp_reduction
    integer, intent(in), optional             :: number_iterations
    real, intent(in), optional                :: scale_factor
    logical, intent(in), optional             :: automatic_scaling
    logical, intent(in), optional             :: verbose

    params%state = 0 ! Initialisation

    if ( .not. update ) then
        params%initial_temp       = 1.0
        params%temp_reduction     = 0.95
        params%scale_factor       = 1.0
        params%number_iterations  = 100
        params%verbose            = .false.
        params%automatic_scaling  = .false.
    endif

    if ( present(initial_temp) ) then
        params%initial_temp       = initial_temp
    endif

    if ( present(temp_reduction) ) then
        params%temp_reduction     = temp_reduction
    endif

    if ( present(scale_factor) ) then
        params%scale_factor       = scale_factor
    endif

    if ( present(number_iterations) ) then
        params%number_iterations  = number_iterations
    endif

    if ( present(automatic_scaling) ) then
        params%automatic_scaling  = automatic_scaling
    endif

    if ( present(verbose) ) then
        params%verbose            = verbose
    endif
end subroutine set_parameters

! determine_new_vector --
!     Determine a new random vector
!
! Arguments:
!     range               Range for the solution parameters
!     x                   New solution parameters
!
subroutine determine_new_vector( range, x )
    real(wp), dimension(2,:), intent(in)      :: range
    real(wp), dimension(:), intent(inout)     :: x

    real(wp)                                  :: y
    integer                                   :: idx

    call random_number( y )
    idx = min( 1 + size(x) * y, size(x) )

    call random_number( y )
    x(idx) = range(1,idx) * (range(2,idx)-range(1,idx)) * y

end subroutine determine_new_vector

! get_next_step --
!     Set the next step in the simulated annealing process
!
! Arguments:
!     params              The structure holding the process parameters
!     range               Range for the solution parameters
!     x                   Present solution parameters
!     value               Function value at present solution
!     task                Task to be run
!
subroutine get_next_step( params, range, x, value, task )
    type(ANNEALING_PARAMETERS), intent(inout) :: params
    real(wp), intent(in)                      :: range
    real(wp), dimension(2,:), intent(in)      :: range
    real(wp), dimension(:), intent(inout)     :: x
    real(wp), intent(in)                      :: value
    integer, intent(inout)                    :: task

    !
    ! Initial stage: automatic scaling required?
    ! Then simply accumulate the function values
    !
    if ( task == annealing_report ) then
        call determine_new_vector( range, x, idx, oldx )
        task = annealing_value
        return
    endif

    if ( params%state == 0 ) then
        if ( params%automatic_scaling ) then

            if ( params%iteration_count < params%number_iterations / 3 ) then
                params%scale_factor    = params%scale_factor    + value
                params%iteration_count = params%iteration_count + 1

                call determine_new_vector( range, x, idx, oldx )
                task = annealing_value
                return
            else
                params%scale_factor    = params%scale_factor    / &
                                         params%iteration_count
            endif
        endif
        params%state           = 1
        params%iteration_count = 0
        params%changes         = .true.
        params%accepted        = 0
    endif

    !
    ! Evaluate the function value and decide what to do now
    !
    call random_number( threshold )
    if ( exp((params%old_value - value)/(params%scale_factor *params%temperature)) > &
            threshold ) then
        params%old_value       = value
        params%changes         = .true.
        params%accepted        = params%accepted + 1
        params%iteration_count = params%iteration_count + 1
    endif

    if ( params%iteration_count >= params%number_iterations ) then
        params%iteration_count = 0
        if ( params%changes ) then
            if ( params%verbose ) then
                task = annealing_report
            else
                task = annealing_value
            endif
            params%temperature = params%temperature * params%temp_reduction
        else
            task = annealing_done
        endif
    endif

end subroutine get_next_step
