! prototypes.f90 --
!     Object-oriented programming using the prototype pattern
!     - see https://steve-yegge.blogspot.com/2008/10/universal-design-pattern.html
!
!     Note:
!     This is a very preliminary implementation, just to show that it works,
!     using unlimited polymorphic parameters.
!
!     TODO:
!     - Turn it into a proper class
!     - Clone/inherit
!     - Delete properties
!     - Make it flexible via a simple hash table
!     - If value already allocated, do we need to deallocate or can we simply
!       assign? (That is: is the type for the unlimited polymorphic variable
!       persistent?)
!
module prototypes
     implicit none

     private

     type property_type
         character(len=:), allocatable :: name
         class(*), allocatable         :: value
     end type property_type

     type prototype
         type(property_type), dimension(10) :: list
     end type prototype

     interface prototype_set
         module procedure prototype_set_int
         module procedure prototype_set_char
     end interface

     interface prototype_get
         module procedure prototype_get_int
         module procedure prototype_get_char
     end interface

     public :: prototype, prototype_set, prototype_print, prototype_get
contains
subroutine find_index( p, name, indx )
    type(prototype), intent(inout) :: p
    character(len=*), intent(in)   :: name
    integer, intent(out)           :: indx

    integer                        :: i

    indx = -1
    do i = 1,size(p%list)
        if ( allocated(p%list(i)%name) ) then
            if ( p%list(i)%name == name ) then
                indx = i
                exit
            endif
        elseif ( indx == -1 ) then
            indx = i
            p%list(indx)%name = name
        endif
    enddo
end subroutine find_index

subroutine find_existing_index( p, name, indx )
    type(prototype), intent(in)  :: p
    character(len=*), intent(in) :: name
    integer, intent(out)         :: indx

    integer                      :: i

    indx = -1
    do i = 1,size(p%list)
        if ( allocated(p%list(i)%name) ) then
            if ( p%list(i)%name == name ) then
                indx = i
                exit
            endif
        endif
    enddo
end subroutine find_existing_index

subroutine prototype_set_int( p, name, value )
    type(prototype), intent(inout) :: p
    character(len=*), intent(in)   :: name
    integer, intent(in)            :: value

    integer :: indx

    call find_index( p, name, indx )

    if ( allocated(p%list(indx)%value) ) then
        deallocate( p%list(indx)%value )
    endif
    allocate( p%list(indx)%value, source = value )
end subroutine prototype_set_int

subroutine prototype_set_char( p, name, value )
    type(prototype), intent(inout) :: p
    character(len=*), intent(in)   :: name
    character(len=*), intent(in)   :: value

    integer :: indx

    call find_index( p, name, indx )

    if ( allocated(p%list(indx)%value) ) then
        deallocate( p%list(indx)%value )
    endif
    allocate( p%list(indx)%value, source = value )
end subroutine prototype_set_char

subroutine prototype_get_int( p, name, value, found )
    type(prototype), intent(in)  :: p
    character(len=*), intent(in) :: name
    integer, intent(out)         :: value
    logical, intent(out)         :: found

    integer :: indx

    call find_existing_index( p, name, indx )

    found = .false.
    value = -999
    if ( indx > -1 ) then
        select type( v => p%list(indx)%value )
            type is (integer)
                found = .true.
                value = v
            class default
                write(*,*) 'Value for "', name, '" not an integer'
        end select
    else
        write(*,*) 'Property "', name, '" does not exist'
    endif
end subroutine prototype_get_int

subroutine prototype_get_char( p, name, value, found )
    type(prototype), intent(in)                :: p
    character(len=*), intent(in)               :: name
    character(len=:), allocatable, intent(out) :: value
    logical, intent(out)                       :: found

    integer :: indx

    call find_existing_index( p, name, indx )

    found = .false.
    value = '???'
    if ( indx > -1 ) then
        select type( v => p%list(indx)%value )
            type is (character(len=*))
                found = .true.
                value = v
            class default
                write(*,*) 'Value for "', name, '" not a character'
        end select
    else
        write(*,*) 'Property "', name, '" does not exist'
    endif
end subroutine prototype_get_char

subroutine prototype_print( p )
    type(prototype), intent(in) :: p

    integer :: indx

    do indx = 1,size(p%list)
        if ( allocated( p%list(indx)%name ) ) then
            select type( v => p%list(indx)%value )
                type is (integer)
                    write(*,'(2a,i0)' ) p%list(indx)%name, ': ', v
                type is (character(len=*))
                    write(*,'(2a,a)' ) p%list(indx)%name, ': ', v
                class default
                    write(*,'(2a,a)' ) p%list(indx)%name, ': ? unknown type'
            end select
        endif
    enddo
end subroutine prototype_print

end module prototypes

! Test --
program test_prototypes
    use prototypes

    type(prototype)               :: p, p2
    logical                       :: found
    integer                       :: int_value
    character(len=:), allocatable :: char_value

    call prototype_set( p, 'Label', 'Some text' )
    call prototype_set( p, 'Label2', 'Some other text' )
    call prototype_set( p, 'Count', 10 )

    call prototype_print( p )

    call prototype_get( p, 'Count', int_value, found )
    call prototype_get( p, 'Label', char_value, found )

    write(*,*) 'Retrieved: ', int_value
    write(*,*) 'Retrieved: ', char_value

    write(*,*) 'Second variable:'
    p2 = p
    call prototype_set( p2, 'Label', 'New label' )
    call prototype_print( p2 )

    write(*,*) 'Original variable:'
    call prototype_print( p )

end program test_prototypes
