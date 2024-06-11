! balanced_tree_string.f90 --
!     Module offering a balanced tree - data are stored with a key that
!     can be compared and ordered, such as an integer value or a string.
!
!     This version uses strings as the data type for the keys
!
!     TODO:
!     - delete_key
!
!     Note:
!     This source file is set up as an include file for constructing an
!     actual module.
!
!     Usage:
!     module balanced_trees
!         use my_module, tree_data => my_data
!         implicit none
!         include "balanced_tree.f90"
!     end module balanced_trees
!
!start module balanced_trees

    private

    public :: tree_data

    type, public :: balanced_tree
        private
        character(len=:), allocatable :: key ! Unallocated strings mean the key was not initialised
        type(tree_data) :: data
        type(balanced_tree), pointer :: left  => null()
        type(balanced_tree), pointer :: right => null()
    contains
        procedure :: add_data    => balanced_add_data
        procedure :: get_data    => balanced_get_data
        procedure :: has_key     => balanced_has_key
        procedure :: traverse    => balanced_traverse
!        procedure :: delete_key  => balanced_delete_key
        procedure :: destroy     => balanced_destroy
    end type balanced_tree

contains

! balanced_add_data --
!     Add a new data item under a (new) key
!
! Arguments:
!     tree                The tree that should store the item
!     key                 The key used to store the data item
!     data                The item itself
!
! Note:
!     If the key+data combination already exists, then
!     the data are simply replaced
!
recursive subroutine balanced_add_data( tree, key, data )
    class(balanced_tree), intent(inout) :: tree
    character(len=*), intent(in)        :: key
    type(tree_data), intent(in)         :: data

    !
    ! New tree?
    !
    if ( .not. allocated(tree%key) ) then
        tree%key  = key
        tree%data = data
        return
    endif

    !
    ! Insert a new key+data left?
    !
    if ( tree%key > key ) then
        if ( associated(tree%left) ) then
            call tree%left%add_data( key, data )
        else
            allocate( tree%left )
            tree%left%key  = key
            tree%left%data = data
        endif
        return
    endif

    !
    ! Insert a new key+data left?
    !
    if ( tree%key < key ) then
        if ( associated(tree%right) ) then
            call tree%right%add_data( key, data )
        else
            allocate( tree%right )
            tree%right%key  = key
            tree%right%data = data
        endif
        return
    endif

    !
    ! Replace the existing data
    !
    if ( tree%key == key ) then
        tree%data = data
    endif
end subroutine balanced_add_data

! balanced_has_key --
!     Check if the tree holds a given key
!
! Arguments:
!     tree                The tree that should store the item
!     key                 The key used to store the data item
!
recursive function balanced_has_key( tree, key ) result(has_key)
    class(balanced_tree), intent(inout)    :: tree
    character(len=*), intent(in)           :: key

    logical                                :: has_key

    has_key = .false.
    if ( tree%key == key ) then
        has_key = .true.
        return
    else
        if ( associated(tree%left) ) then
            has_key = tree%left%has_key(key)
        endif
        if ( .not. has_key .and. associated(tree%right) ) then
            has_key = tree%right%has_key(key)
        endif
    endif
end function balanced_has_key

! balanced_get_data --
!     Get the data that are stored under the given key
!
! Arguments:
!     tree                The tree that should store the item
!     key                 The key used to store the data item
!     data                The data stored under the key
!     success             Whether the key exists
!
! Note:
!     If the key is not present, then the OLD data will be returned
!     untouched.
!
recursive subroutine balanced_get_data( tree, key, data, success )
    class(balanced_tree), intent(inout)    :: tree
    character(len=*), intent(in)           :: key
    type(tree_data), intent(inout)         :: data
    logical, intent(out)                   :: success

    success = .false.
    if ( tree%key == key ) then
        success = .true.
        data    = tree%data
        return
    else
        if ( associated(tree%left) ) then
            call tree%left%get_data(key, data, success)
        endif
        if ( .not. success .and. associated(tree%right) ) then
            call tree%right%get_data(key, data, success)
        endif
    endif
end subroutine balanced_get_data

! balanced_destroy --
!     Destroy the entire tree
!
! Arguments:
!     tree                The tree to be destroyed
!
! Note:
!     If the tree_data structure contains allocated/pointer
!     components, a dedicated finalise routine for this derived type
!     should take care of them
!
recursive subroutine balanced_destroy( tree )
    class(balanced_tree), intent(inout)    :: tree

    if ( associated(tree%left) ) then
        call tree%left%destroy
        deallocate( tree%left )
        tree%left => null()
    endif
    if ( associated(tree%right) ) then
        call tree%right%destroy
        deallocate( tree%right )
        tree%right => null()
    endif

    deallocate( tree%key )

end subroutine balanced_destroy

! balanced_traverse --
!     Traverse the entire tree, running a user-defined routine on
!     each node
!
! Arguments:
!     tree                The tree to be traversed
!     routine             The routine to be applied to each node
!
! Note:
!     The routine is not supposed to change anything about the key
!     or the data that are associated with it
!
recursive subroutine balanced_traverse( tree, routine )
    class(balanced_tree), intent(inout)    :: tree

    interface
        subroutine routine( key, data )
            import tree_data
            character(len=*), intent(in) :: key
            type(tree_data), intent(in)  :: data
        end subroutine routine
    end interface

    call routine( tree%key, tree%data )

    if ( associated(tree%left) ) then
        call tree%left%traverse( routine )
    endif
    if ( associated(tree%right) ) then
        call tree%right%traverse( routine )
    endif
end subroutine balanced_traverse

!end module balanced_trees
