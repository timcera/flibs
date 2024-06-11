! simple_linkedlist.f90 --
!     Very basic implementation of linked lists
!     Main purpose: illustrate the use of templates
!
!     Real and real, dimension(:), allocatable
!
!     Pure module implementation
!
!!template linked_lists
!!    implicit none
!!
!!    private
!!    public :: linked_list_def
!!
!!    type linked_list_def
!!        type(linked_list_def), pointer :: next
!!        type(data_type)                :: data
!!    contains
!!        ! To do
!!        procedure :: print => print_list
!!    end type linked_list_def
!!contains
!!    ! To do
!!
!!    print_list uses print_item
!!end template
!!
module linked_list_reals
    use_template linked_lists, real => data_type, linked_list_real => linked_list_def
    implicit none

    private
    public :: linked_list_real

    type linked_list_real
        type(linked_list_real), pointer :: next => null()
        real                            :: data
    contains
        procedure :: add => add_to_list
        procedure :: print => print_list
    end type linked_list_real
contains
subroutine add_to_list( list, data )
    class(linked_list_real), intent(inout) :: list
    real, intent(in)                      :: data

    type(linked_list_real), pointer       :: item

    allocate( item )
    item%data = data

    item%next => list%next
    list%next => item
end subroutine add_to_list

subroutine print_list( list, lun )
    class(linked_list_real), intent(in), target :: list
    integer, intent(in)                        :: lun

    type(linked_list_real), pointer            :: item
    integer                                    :: i

    i = 1
    item => list
    do
        call print_item( lun, i, item%data )

        if ( associated( item%next ) ) then
            i = i + 1
            item => item%next
        else
            exit
        endif
    enddo
end subroutine print_list

subroutine print_item( lun, indx, data )
    integer, intent(in) :: lun
    integer, intent(in) :: indx
    real, intent(in)    :: data

    write( lun, '(i5,a,e14.5)' ) indx, ':', data
end subroutine print_item
end module linked_list_reals

module linked_list_multi_reals
    use_template linked_lists, real, dimension(:), allocatable => data_type, linked_list_multireal => linked_list_def
contains
subroutine print_item( lun, indx, data )
    integer, intent(in) :: lun
    integer, intent(in) :: indx
    real, intent(in)    :: data

    write( lun, '(i5,a,5e14.5,/,(6x,5e14.5))' ) indx, ':', data
end subroutine print_item
end module linked_list_multireals

program test_linked_list
    use linked_list_reals

    type(linked_list_real) :: mylist
    integer                :: i
    real                   :: value

    do i = 1,10
        value = 0.1 * i
        call mylist%add( value )
    enddo

    open( 20, file = 'test_linked_list.out' )
    call mylist%print(20)
end program test_linked_list
