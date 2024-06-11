! lambda.f90
!
!     Experiment with lambda expressions (that is, anonymous functions) in
!     Fortran
!
!     TODO: finalisers
!
!     Hm, I do not quite like the treatment of "expr" in correct_pointer:
!     "Check the routine correct_pointer! There is something fishy as the
!     pointer to the original expression is lost and possibly at the
!     wrong moment if there is more than one free variable."
!
module lambda_expressions
    implicit none

    private

    type lambda_integer
        integer                       :: operation
        integer                       :: value
        type(lambda_integer), pointer :: first  => null()
        type(lambda_integer), pointer :: second => null()
    end type lambda_integer

    type lambda_integer_pointer
        type(lambda_integer), pointer :: arg => null()
    end type lambda_integer_pointer

    type lambda_expression
        type(lambda_integer_pointer)      :: arg(4)
        type(lambda_integer)              :: operand(4)
        type(lambda_integer), pointer     :: expr
    contains
        procedure :: set  => set_expression
        procedure :: eval => eval_expression
    end type lambda_expression

    interface assignment(=)
        module procedure integer_set_value
    end interface

    interface operator(+)
        module procedure integer_add
        module procedure integer_add_v
        module procedure integer_add_vv
    end interface

    interface operator(*)
        module procedure integer_multiply
        module procedure integer_multiply_v
        module procedure integer_multiply_vv
    end interface

    interface operator(**)
        module procedure integer_exponentiate_v
    end interface

    public lambda_integer, lambda_expression, assignment(=), operator(+), &
           operator(*), operator(**)

contains

subroutine integer_set_value( x, value )
    type(lambda_integer), intent(out) :: x
    integer, intent(in)                :: value

    x%operation = 0
    x%value     = value
    x%first     => null()
    x%second    => null()
end subroutine integer_set_value

function integer_add( x, y ) result(add)
    type(lambda_integer), intent(in), target  :: x
    type(lambda_integer), intent(in), target  :: y
    type(lambda_integer), pointer             :: add

    allocate( add )

    add%operation =  1
    add%first     => x
    add%second    => y
end function integer_add

function integer_add_v( x, y ) result(add)
    type(lambda_integer), intent(in), target  :: x
    integer, intent(in)                       :: y
    type(lambda_integer), pointer             :: add

    type(lambda_integer), pointer             :: yy

    allocate( yy )
    yy = y

    allocate( add )

    add%operation =  1
    add%first     => x
    add%second    => yy
end function integer_add_v

function integer_add_vv( x, y ) result(add)
    integer, intent(in)                       :: x
    type(lambda_integer), intent(in), target  :: y
    type(lambda_integer), pointer             :: add

    type(lambda_integer), pointer             :: xx

    allocate( xx )
    xx = x

    allocate( add )

    add%operation =  1
    add%first     => xx
    add%second    => y
end function integer_add_vv

function integer_multiply( x, y ) result(multiply)
    type(lambda_integer), intent(in), target  :: x
    type(lambda_integer), intent(in), target  :: y
    type(lambda_integer), pointer             :: multiply

    allocate( multiply )

    multiply%operation =  2
    multiply%first     => x
    multiply%second    => y
end function integer_multiply

function integer_multiply_v( x, y ) result(multiply)
    type(lambda_integer), intent(in), target  :: x
    integer, intent(in)                       :: y
    type(lambda_integer), pointer             :: multiply

    type(lambda_integer), pointer             :: yy

    allocate( yy )
    yy = y

    allocate( multiply )

    multiply%operation =  2
    multiply%first     => x
    multiply%second    => yy
end function integer_multiply_v

function integer_multiply_vv( x, y ) result(multiply)
    integer, intent(in)                       :: x
    type(lambda_integer), intent(in), target  :: y
    type(lambda_integer), pointer             :: multiply

    type(lambda_integer), pointer             :: xx

    allocate( xx )
    xx = x

    allocate( multiply )

    multiply%operation =  2
    multiply%first     => xx
    multiply%second    => y
end function integer_multiply_vv

function integer_exponentiate_v( x, y ) result(exponentiate)
    type(lambda_integer), intent(in), target  :: x
    integer, intent(in)                       :: y
    type(lambda_integer), pointer             :: exponentiate

    type(lambda_integer), pointer             :: yy

    allocate( yy )
    yy = y

    allocate( exponentiate )

    exponentiate%operation =  3
    exponentiate%first     => x
    exponentiate%second    => yy
end function integer_exponentiate_v

recursive subroutine integer_eval( x )
    type(lambda_integer)   :: x

    if ( associated( x%first  ) ) call integer_eval( x%first  )
    if ( associated( x%second ) ) call integer_eval( x%second )

    select case( x%operation )
    case ( 0 )
        ! Nothing to be done

    case ( 1 )
        x%value = x%first%value + x%second%value

    case ( 2 )
        x%value = x%first%value * x%second%value

    case ( 3 )
        x%value = x%first%value ** x%second%value

    case default
        ! Nothing to be done
    end select

end subroutine integer_eval

subroutine set_expression( lambda, x, expr )
    class(lambda_expression)      :: lambda
    type(lambda_integer), target  :: x
    type(lambda_integer), pointer :: expr

    type(lambda_integer_pointer), dimension(size(lambda%operand)) :: arg

    arg(1)%arg => x
    arg(2)%arg => null()
    arg(3)%arg => null()
    arg(4)%arg => null()

    !
    ! Correct the pointers to arguments
    !
    call correct_pointer( arg, lambda%operand, expr )
    allocate( lambda%expr, source=expr )
end subroutine set_expression

recursive subroutine correct_pointer( arg, operand, expr )
    type(lambda_integer_pointer), dimension(:) :: arg
    type(lambda_integer), dimension(:), target :: operand
    type(lambda_integer), pointer              :: expr
    integer :: ierr

    integer :: i

    !
    ! This is fishy!
    !
    do i = 1,size(arg)
        if ( associated(arg(i)%arg, expr ) ) then
            expr => operand(i)

            if ( associated(expr%first) ) then
                call correct_pointer( arg, operand, expr%first )
            endif
            if ( associated(expr%second) ) then
                call correct_pointer( arg, operand, expr%second )
            endif
        endif
    enddo
end subroutine correct_pointer

integer function eval_expression( lambda, x )
    class(lambda_expression)     :: lambda
    integer                      :: x

    lambda%operand(1)%value = x

    call integer_eval( lambda%expr )

    eval_expression = lambda%expr%value
end function eval_expression

end module lambda_expressions

program test_lambda
    use lambda_expressions

    type(lambda_integer)           :: x
    type(lambda_expression)        :: lambda1, lambda2, lambda3
    integer                        :: v

    call lambda1%set( x, x+2 )
    call lambda2%set( x, x*2 )
    call lambda3%set( x, x*2+x )

    do v = 1,10
        write(*,*) v, lambda1%eval(v), lambda2%eval(v), lambda3%eval(v)
    enddo
end program
