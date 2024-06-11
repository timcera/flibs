! test_balanced_tree.f90 --
!     Test program for balanced trees, as well as a demonstration of how
!     the include file can be used
!
!     TODO:
!     - delete_key
!
module my_data
    type, public :: point2d
        real :: x, y
    end type point2d

    type, public :: point3d
        real :: x, y, z
    end type point3d
end module my_data

!
! Define a module for trees that hold 2D points
!
module balanced_trees_2d_points
    use my_data, tree_data => point2d
    implicit none

    public :: print_tree

    include "balanced_tree.f90"

! Extra:
! Debug routine to test the implementation
!
recursive subroutine print_tree( tree, indent )
    class(balanced_tree), intent(inout)    :: tree
    character(len=*), intent(in)           :: indent

    write(*,'(2a,i0,'' - '',2f10.2)') indent, 'Tree: ', tree%key, tree%data
    write(*,'(2a,l)') indent, '    Left:  ', associated(tree%left)
    write(*,'(2a,l)') indent, '    Right: ', associated(tree%right)

    if ( associated(tree%left) ) then
        call print_tree(tree%left, indent // '    ')
    endif
    if ( associated(tree%right) ) then
        call print_tree(tree%right, indent // '    ')
    endif
end subroutine print_tree

end module balanced_trees_2d_points

!
! Define a module for trees that hold 3D points
! No need for a print routine
!
module balanced_trees_3d_points
    use my_data, tree_data => point3d
    implicit none

    include "balanced_tree.f90"

end module balanced_trees_3d_points

!
! Define an overall module so that we have convenient
! names
!
module balanced_trees
    use balanced_trees_2d_points, btree_2d => balanced_tree, point2d => tree_data
    use balanced_trees_3d_points, btree_3d => balanced_tree, point3d => tree_data
end module balanced_trees

! Test program
!
program test_balanced_trees
    use balanced_trees

    implicit none

    integer             :: key
    type(point2d)       :: p2d
    type(point3d)       :: p3d      ! Not actually used, just an illustration
    type(btree_2d)      :: tree
    type(btree_3d)      :: tree3d   ! Ditto
    logical             :: success

    key = 30
    p2d%x = 1
    p2d%y = 30
    call tree%add_data(key, p2d)

    key = 10
    p2d%y = 10
    call tree%add_data(key, p2d)

    key = 20
    p2d%y = 20
    call tree%add_data(key, p2d)

    key = 40
    p2d%y = 40
    call tree%add_data(key, p2d)

    key = 35
    p2d%y = 35
    call tree%add_data(key, p2d)

    key = 20
    p2d%y = 40
    call tree%add_data(key, p2d)

    call print_tree(tree, '' )

    write(*,*) 'Key 30? ', tree%has_key(30)
    write(*,*) 'Key 31? ', tree%has_key(31)

    key = 35
    p2d%y = 40 ! Should be changed
    call tree%get_data( key, p2d, success )
    write(*,*) 'Key 35: ', success, ' - ', p2d%y

    key = 31
    p2d%y = 33 ! Should be NOT changed
    call tree%get_data( key, p2d, success )
    write(*,*) 'Key 31: ', success, ' - ', p2d%y

    !
    ! Now print the contents
    !
    call tree%traverse( print_p2d )

    call tree%destroy
contains
subroutine print_p2d( key, p2d )
    integer, intent(in)       :: key
    type(point2d), intent(in) :: p2d

    write(*,*) 'Key:  ', key
    write(*,*) '    Data X: ', p2d%x
    write(*,*) '    Data Y: ', p2d%y
end subroutine print_p2d

end program test_balanced_trees
