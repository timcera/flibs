!
! m_vstringlist.f90
!   This module provides services to manage lists of 
!   vstrings.
!
!   A list of strings can be created with vstrlist_new and 
!   several kinds of input arguments. With no additionnal arguments,
!   the vstrlist_new method creates a list with no items :
!   
!      call vstrlist_new ( list )
!
!   If one gives one string, the list is created with 1 element.
!   One can also give an array of strings to the vstrlist_new method.
!   The list is destroyed with vstrlist_free.
!   The number of elements in the list can be computed with 
!   vstrlist_length while vstrlist_exists allows to known 
!   if a list has been created.
!
!   Several methods are provided to acces to the elements of one list 
!   of strings, for example vstrlist_index, vstrlist_range and vstrlist_set.
!   The vstrlist_set method allows to set the element at a given index.
!   The strindex-th item of the list can be accessed with vstrlist_index :
!
!     string1 = vstrlist_index ( list , 2 )
!
!   The vstrlist_range method returns a new list made of the elements
!   which index is between two given integers :
!
!     list2 = vstrlist_range ( list , 2 , 3 )
!
!   To add items to a list, one can use vstrlist_append, vstrlist_concat 
!   or vstrlist_insert.
!   The vstrlist_concat and vstrlist_insert methods return a new list 
!   while vstrlist_append add one item to an existing list (therefore 
!   increasing the number of elements in the list). In the following 
!   example, a new list made of 3 items is created :
!
!     call vstrlist_new ( list )
!     call vstrlist_append ( list , "fortran 77" )
!     call vstrlist_append ( list , "fortran 90" )
!     call vstrlist_append ( list , "fortran 95" )
!     call vstrlist_append ( list , "fortran 2003" )
!     call vstrlist_free ( list )
!
!   The vstrlist_split method allows to split a vstring into an list  
!   of vstrings each time one character is found in a vstring. 
!   The vstrlist_join method concatenates an list of vstrings,
!   using a vstring as the join between the components.
!   In the following example, the string is split at each dot 
!   and the number of components is 3 :
!
!     call vstring_new ( string1 , "comp.lang.fortran" )
!     strlist = vstrlist_split ( string1 , "." )
!
!   One can search for a pattern in a list. The vstrlist_search
!   returns the index of the found string while vstrlist_lsearch
!   returns the list of matching strings. These methods 
!   are based on vstring_match and therefore are powerfull 
!   tools to process strings.
!   In the following example, one searches for all fortran
!   compilers from the 90s (based on the previous sample list):
!
!     list2 = vstrlist_search ( list , "fortran 9*" )
!
! Copyright (c) 2008 Michael Baudin
!   
module m_vstringlist
  use m_vstring, only : &
       t_vstring , &
       vstring_new , &
       vstring_free , &
       vstring_set_stoponerror ,&
       VSTRING_WHITESPACE ,&
       vstring_length , &
       vstring_index , &
       vstring_first , &
       vstring_append , &
       VSTRING_SPACE , &
       vstring_match , &
       vstring_equals
  implicit none
  private
  !
  ! Public methods
  !
  public :: vstrlist_new
  public :: vstrlist_free
  public :: vstrlist_length
  public :: vstrlist_exists
  public :: vstrlist_reference_get
  public :: vstrlist_index
  public :: vstrlist_append
  public :: vstrlist_concat
  public :: vstrlist_insert
  public :: vstrlist_range
  public :: vstringlist_set_stoponerror
  public :: vstrlist_set
  public :: vstrlist_split
  public :: vstrlist_join
  public :: vstrlist_search
  public :: vstrlist_lsearch
  !
  ! t_vstringlist --
  !   A list of vstrings is implemented as an array of vstrings.
  !
  ! Choose your dynamic string system between _VSTRINGLIST_ALLOCATABLE , _VSTRINGLIST_POINTER
  ! Allocatable arrays should be used by default.
  ! But for compatibility of older fortran 90 compilers, pointers are also available.
  ! These are the recommended settings :
  ! _VSTRING_POINTER : gfortran, g95
  ! _VSTRINGLIST_POINTER : Intel Fortran 8.0 (allocatable does not work)
  !
  type, public :: t_vstringlist
     private
#ifdef _VSTRINGLIST_ALLOCATABLE
     type ( t_vstring ) , dimension (:), allocatable :: array
#endif
#ifdef _VSTRINGLIST_POINTER
     type ( t_vstring ) , dimension (:), pointer :: array
#endif
  end type t_vstringlist
  !
  ! vstrlist_new --
  !   Generic interface for the constructor.
  !
  interface vstrlist_new
     module procedure vstrlist_new_from_empty
     module procedure vstrlist_new_from_string
     module procedure vstrlist_new_from_charstring
     module procedure vstrlist_new_from_array
     module procedure vstrlist_new_from_list
     module procedure vstrlist_new_from_integer
  end interface vstrlist_new
  !
  ! vstrlist_append --
  !   Generic interface to append a string or a list.
  !
  interface vstrlist_append
     module procedure vstrlist_append_string
     module procedure vstrlist_append_charstring
     module procedure vstrlist_append_list
  end interface vstrlist_append
  !
  ! vstrlist_concat --
  !   Generic interface to concatenate a string or a list.
  !
  interface vstrlist_concat
     module procedure vstrlist_concat_string
     module procedure vstrlist_concat_charstring
     module procedure vstrlist_concat_list
  end interface vstrlist_concat
  !
  ! vstrlist_insert --
  !   Generic interface to insert a string into the list.
  !
  interface vstrlist_insert
     module procedure vstrlist_insert_string
     module procedure vstrlist_insert_charstring
  end interface vstrlist_insert
  !
  ! vstrlist_set --
  !   Generic interface to set a string in a list.
  !
  interface vstrlist_set
     module procedure vstrlist_set_vstring
     module procedure vstrlist_set_charstring
  end interface vstrlist_set
  !
  ! vstrlist_split_charstring --
  !   Generic interface to split a string into a list.
  !
  interface vstrlist_split
     module procedure vstrlist_split_vstring
     module procedure vstrlist_split_charstring
  end interface vstrlist_split
  !
  ! vstrlist_join --
  !   Generic interface to join a list into a string.
  !
  interface vstrlist_join
     module procedure vstrlist_join_vstring
     module procedure vstrlist_join_charstring
  end interface vstrlist_join
  !
  ! vstrlist_search --
  !   Generic interface to search a string in a list.
  !
  interface vstrlist_search
     module procedure vstrlist_search_vstring
     module procedure vstrlist_search_charstring
  end interface vstrlist_search
  !
  ! vstrlist_lsearch --
  !   Generic interface to compute the list of items which match a pattern.
  !
  interface vstrlist_lsearch
     module procedure vstrlist_lsearch_vstring
     module procedure vstrlist_lsearch_charstring
  end interface vstrlist_lsearch
  !
  ! Total number of currently available (allocated) lists.
  ! Note :
  ! This is mainly for debugging purposes of the vstringlist module itself or client algorithms.
  ! It allows to check the consistency of vstring_new/vstring_free statements.
  integer, save :: vstringlist_number_of_lists = 0
  !
  ! Set to true to stop whenever an error comes in the vstringlist component.
  logical, save :: vstringlist_stoponerror = .true.
  !
  ! Flags for error management.
  !
  integer, parameter :: VSTRINGLIST_ERROR_OK = 0
  integer, parameter :: VSTRINGLIST_ERROR_INDEXWRONG = 1
  !
  ! Constants
  !
  integer, parameter, public :: VSTRINGLIST_INDEX_UNKNOWN = 0
contains
  !
  ! vstrlist_new_from_empty --
  !   Constructor for an empty list of strings
  !
  subroutine vstrlist_new_from_empty ( this )
    type ( t_vstringlist ) , intent(inout) :: this
    allocate ( this % array ( 0 ) )
    !
    ! Update the counter of lists
    !
    call vstrlist_reference_add ()
  end subroutine vstrlist_new_from_empty
  !
  ! vstrlist_new_from_string --
  !   Constructor from one vstring
  !
  subroutine vstrlist_new_from_string ( this , string )
    type ( t_vstringlist ) , intent(inout) :: this
    type ( t_vstring ) , intent(in) :: string
    allocate ( this % array ( 1 ) )
    call vstring_new ( this % array ( 1 ) , string )
    !
    ! Update the counter of lists
    !
    call vstrlist_reference_add ()
  end subroutine vstrlist_new_from_string
  !
  ! vstrlist_new_from_charstring --
  !   Constructor from one character string
  !
  subroutine vstrlist_new_from_charstring ( this , string )
    type ( t_vstringlist ) , intent(inout) :: this
    character ( len = * ) , intent(in) :: string
    type ( t_vstring ) :: vstring
    call vstring_new ( vstring , string )
    call vstrlist_new_from_string ( this , vstring )
    call vstring_free ( vstring )
  end subroutine vstrlist_new_from_charstring
  !
  ! vstrlist_new_from_array --
  !   Constructor from an array of vstrings
  !
  subroutine vstrlist_new_from_array ( this , array )
    type ( t_vstringlist ) , intent(inout) :: this
    type ( t_vstring ) , dimension (:), intent(in) :: array
    integer :: length
    integer :: icomponent
    length = size ( array )
    allocate ( this % array ( length ) )
    do icomponent = 1 , length
       call vstring_new ( this % array ( icomponent ) , array ( icomponent ) )
    enddo
    !
    ! Update the counter of lists
    !
    call vstrlist_reference_add ()
  end subroutine vstrlist_new_from_array
  !
  ! vstrlist_new_from_list --
  !   Constructor from a list of vstrings
  !
  subroutine vstrlist_new_from_list ( this , list )
    type ( t_vstringlist ) , intent(inout) :: this
    type ( t_vstringlist ), intent(in) :: list
    integer :: length
    integer :: icomponent
    length = vstrlist_length ( list )
    allocate ( this % array ( length ) )
    do icomponent = 1 , length
       call vstring_new ( this % array ( icomponent ) , list % array ( icomponent ) )
    enddo
    !
    ! Update the counter of lists
    !
    call vstrlist_reference_add ()
  end subroutine vstrlist_new_from_list
  !
  ! vstrlist_new_from_integer --
  !   Constructor for list of n empty strings
  !
  subroutine vstrlist_new_from_integer ( this , length )
    type ( t_vstringlist ) , intent(inout) :: this
    integer , intent(in) :: length
    character ( len = 400 ) :: message
    integer :: icomponent
    if ( length < 0 ) then
       write ( message , * ) "The given length ", length, " is lower than 0."
       call vstring_error ( this , message )
    endif
    allocate ( this % array ( length ) )
    do icomponent = 1 , length
       call vstring_new ( this % array ( icomponent ) )
    enddo
    !
    ! Update the counter of lists
    !
    call vstrlist_reference_add ()
  end subroutine vstrlist_new_from_integer
  !
  ! vstrlist_free --
  !   Destructor for a list of strings
  !
  subroutine vstrlist_free ( this )
    type ( t_vstringlist ) , intent(inout) :: this
    integer :: length
    integer :: icomponent
    length = vstrlist_length ( this )
    do icomponent = 1 , length
       call vstring_free ( this % array ( icomponent ) )
    enddo
    deallocate ( this % array )
    call vstrlist_reference_remove ()
  end subroutine vstrlist_free
  !
  ! vstrlist_reference_add --
  !   Static method.
  !   Increase the counter of currently referenced lists.
  !
  subroutine vstrlist_reference_add ( )
    implicit none
    vstringlist_number_of_lists = vstringlist_number_of_lists + 1
  end subroutine vstrlist_reference_add
  !
  ! vstrlist_reference_remove --
  !   Static method.
  !   Decrease the counter of currently referenced lists.
  !
  subroutine vstrlist_reference_remove ( )
    implicit none
    vstringlist_number_of_lists = vstringlist_number_of_lists - 1
  end subroutine vstrlist_reference_remove
  !
  ! vstrlist_reference_get --
  !   Static method.
  !   Returns the number of currently referenced lists.
  !
  integer function vstrlist_reference_get ( )
    implicit none
    vstrlist_reference_get = vstringlist_number_of_lists
  end function vstrlist_reference_get
  !
  ! vstrlist_length --
  !   Returns the number of elements in the list.
  !
  integer function vstrlist_length ( this )
    type ( t_vstringlist ) , intent(in) :: this
    vstrlist_length = size ( this % array )
  end function vstrlist_length
  !
  ! vstrlist_exists --
  !   Returns .true. if the current list has been created.
  !
  logical function vstrlist_exists ( this )
    type ( t_vstringlist ) , intent(in) :: this
#ifdef _VSTRINGLIST_ALLOCATABLE
    vstrlist_exists = allocated ( this % array )
#endif
#ifdef _VSTRINGLIST_POINTER
    vstrlist_exists = associated ( this % array )
#endif
  end function vstrlist_exists
  !
  ! vstrlist_index --
  !   Creates a new vstring by getting the vstring at the given index in the list.
  !   Generates an error if the given index does not exist.
  !
  function vstrlist_index ( this , icomponent ) result ( newstring )
    type ( t_vstringlist ) , intent(in) :: this
    type ( t_vstring ) :: newstring
    integer :: icomponent
    integer :: status
    call vstrlist_check_index ( this , icomponent , status )
    if ( status /= VSTRINGLIST_ERROR_OK ) then
       return
    endif
    call vstring_new ( newstring , this % array ( icomponent ) )
  end function vstrlist_index
  !
  ! vstrlist_append_string --
  !   Add the given vstring at the end of the current list.
  !
  subroutine vstrlist_append_string ( this , string )
    type ( t_vstringlist ) , intent(inout) :: this
    type ( t_vstring ) , intent(in) :: string
    type ( t_vstringlist ) :: oldlist
    call vstrlist_new ( oldlist , this )
    call vstrlist_free ( this )
    this = vstrlist_concat ( oldlist , string )
    call vstrlist_free ( oldlist )
  end subroutine vstrlist_append_string
  !
  ! vstrlist_append_charstring --
  !   Interface to vstrlist_append_string to manage character strings.
  !
  subroutine vstrlist_append_charstring ( this , string )
    type ( t_vstringlist ) , intent(inout) :: this
    character(len=*) , intent(in) :: string
    type ( t_vstring ) :: vstring
    call vstring_new ( vstring , string )
    call vstrlist_append_string ( this , vstring )
    call vstring_free ( vstring )
  end subroutine vstrlist_append_charstring
  !
  ! vstrlist_append_list --
  !   Add the given list at the end of the current list.
  !
  subroutine vstrlist_append_list ( this , list )
    type ( t_vstringlist ) , intent(inout) :: this
    type ( t_vstringlist ) , intent(in) :: list
    type ( t_vstringlist ) :: oldlist
    call vstrlist_new ( oldlist , this )
    call vstrlist_free ( this )
    this = vstrlist_concat ( oldlist , list )
    call vstrlist_free ( oldlist )
  end subroutine vstrlist_append_list
  !
  ! vstrlist_concat_string --
  !   Creates a new list by concatenating the current list to the given string.
  ! Arguments
  !   string : the string to be concatenated
  !   newlist : the concatenated list
  !
  function vstrlist_concat_string ( this , string ) result ( newlist )
    type ( t_vstringlist ) , intent(in) :: this
    type ( t_vstring ) , intent(in) :: string
    type ( t_vstringlist ) :: newlist
    ! Local variables
    integer :: newlength
    integer :: oldlength
    integer :: icomponent
    !
    ! Compute the new length and create the new list.
    !
    oldlength = vstrlist_length ( this )
    newlength = oldlength + 1
    call vstrlist_new ( newlist , newlength )
    !
    ! Fill the new list
    !
    do icomponent = 1 , oldlength
       call vstring_free ( newlist % array ( icomponent ) )
       call vstring_new ( newlist % array ( icomponent )  , this % array ( icomponent ) )
    enddo
    call vstring_free ( newlist % array ( newlength ) )
    call vstring_new ( newlist % array ( newlength )  , string )
  end function vstrlist_concat_string
  !
  ! vstrlist_concat_charstring --
  !   Interface to vstrlist_concat_string to manage character strings.
  !
  function vstrlist_concat_charstring ( this , string ) result ( newlist )
    type ( t_vstringlist ) , intent(in) :: this
    character (len=*), intent(in) :: string
    type ( t_vstringlist ) :: newlist
    type ( t_vstring ) :: vstring
    call vstring_new ( vstring , string )
    newlist = vstrlist_concat_string ( this , vstring )
    call vstring_free ( vstring )
  end function vstrlist_concat_charstring
  !
  ! vstrlist_concat_list --
  !   Returns a new list by concatenating the current list to the given list.
  ! Arguments
  !   list : the list to be concatenated
  !   newlist : the concatenated list
  !
  function vstrlist_concat_list ( this , list ) result ( newlist )
    type ( t_vstringlist ) , intent(in) :: this
    type ( t_vstringlist ) , intent(in) :: list
    type ( t_vstringlist ) :: newlist
    ! Local variables
    integer :: newlength
    integer :: oldlength
    integer :: listlength
    integer :: icomponent
    integer :: icomponent_reduced
    !
    ! Compute the new length and create the new list.
    !
    oldlength = vstrlist_length ( this )
    listlength = vstrlist_length ( list )
    newlength = oldlength + listlength
    call vstrlist_new ( newlist , newlength )
    !
    ! Fill the new list
    !
    do icomponent = 1 , oldlength
       call vstring_free ( newlist % array ( icomponent ) )
       call vstring_new ( newlist % array ( icomponent )  , this % array ( icomponent ) )
    enddo
    do icomponent = oldlength + 1 , newlength
       icomponent_reduced = icomponent - oldlength
       call vstring_free ( newlist % array ( icomponent ) )
       call vstring_new ( newlist % array ( icomponent )  , list % array ( icomponent_reduced ) )
    enddo
  end function vstrlist_concat_list
  !
  ! vstrlist_insert_string --
  !   Creates a new list by inserting the given string into the current list just 
  !   before the given index "strindex".
  ! Arguments
  !   string : the string to be insert
  !   strindex : the index where the string must be inserted
  !   newlist : the new list
  !
  function vstrlist_insert_string ( this , strindex, string ) result ( newlist )
    type ( t_vstringlist ) , intent(in) :: this
    type ( t_vstring ) , intent(in) :: string
    integer , intent ( in ) :: strindex
    type ( t_vstringlist ) :: newlist
    ! Local variables
    integer :: newlength
    integer :: oldlength
    integer :: icomponent
    integer :: icomponent_reduced
    integer :: status
    type ( t_vstring ) :: this_string
    oldlength = vstrlist_length ( this )
    if ( oldlength == 0 .AND. strindex == 1 ) then
       ! No problem : we try to insert a string at the begining of an empty list
    else
       !
       ! Check the index
       !
       call vstrlist_check_index ( this , strindex , status )
       if ( status /= VSTRINGLIST_ERROR_OK ) then
          return
       endif
    endif
    !
    ! Compute the new length and create the new list.
    !
    newlength = oldlength + 1
    call vstrlist_new ( newlist , newlength )
    !
    ! Fill the new list
    !
    do icomponent = 1 , strindex - 1
       call vstring_free ( newlist % array ( icomponent ) )
       this_string = vstrlist_index ( this , icomponent )
       call vstring_new ( newlist % array ( icomponent )  , this_string )
       call vstring_free ( this_string )
    enddo
    call vstring_free ( newlist % array ( strindex ) )
    call vstring_new ( newlist % array ( strindex )  , string )
    do icomponent = strindex + 1 , newlength
       icomponent_reduced = icomponent - 1
       call vstring_free ( newlist % array ( icomponent ) )
       this_string = vstrlist_index ( this , icomponent_reduced )
       call vstring_new ( newlist % array ( icomponent )  , this_string )
       call vstring_free ( this_string )
    enddo
  end function vstrlist_insert_string
  !
  ! vstrlist_insert_charstring --
  !   Interface to vstrlist_insert_string to manage character strings.
  !
  function vstrlist_insert_charstring ( this , strindex , string ) result ( newlist )
    type ( t_vstringlist ) , intent(in) :: this
    character (len=*), intent(in) :: string
    integer , intent(in) :: strindex
    type ( t_vstringlist ) :: newlist
    type ( t_vstring ) :: vstring
    call vstring_new ( vstring , string )
    newlist = vstrlist_insert_string ( this , strindex , vstring )
    call vstring_free ( vstring )
  end function vstrlist_insert_charstring
  !
  ! vstrlist_range --
  !   Creates a new list by extracting items of index  
  !   from first and last (included).
  ! Arguments
  !   first : the index of the first element to include
  !   last : the index of the last element to include
  !
  function vstrlist_range ( this , first , last ) result ( newlist )
    type ( t_vstringlist ) , intent(in) :: this
    integer , intent(in) :: first
    integer , intent(in) :: last
    type ( t_vstringlist ) :: newlist
    integer :: icomponent
    type ( t_vstring ) :: string
    call vstrlist_new ( newlist )
    do icomponent = first , last
       string = vstrlist_index ( this , icomponent )
       call vstrlist_append ( newlist , string )
       call vstring_free ( string )
    enddo
  end function vstrlist_range
  !
  ! vstrlist_set_vstring --
  !   Set the newstring vstring argument at the given strindex,
  !   replacing the existing vstring by the new one.
  ! Arguments
  !   strindex : the index where to put the new string
  !   newstring : the new string
  !
  subroutine vstrlist_set_vstring ( this , strindex , newstring )
    type ( t_vstringlist ) , intent(inout) :: this
    integer , intent(in) :: strindex
    type ( t_vstring ) , intent(in) :: newstring
    integer :: status
    call vstrlist_check_index ( this , strindex , status )
    if ( status /= VSTRINGLIST_ERROR_OK ) then
       return
    endif
    call vstring_free ( this % array ( strindex ) )
    call vstring_new ( this % array ( strindex ) , newstring )
  end subroutine vstrlist_set_vstring
  !
  ! vstrlist_set_charstring --
  !   Interface to vstrlist_set_vstring to manage character strings.
  !
  subroutine vstrlist_set_charstring ( this , strindex , newstring )
    type ( t_vstringlist ) , intent(inout) :: this
    integer , intent(in) :: strindex
    character(len=*) , intent(in) :: newstring
    type ( t_vstring ) :: newvstring
    call vstring_new ( newvstring , newstring )
    call vstrlist_set_vstring ( this , strindex , newvstring )
    call vstring_free ( newvstring )
  end subroutine vstrlist_set_charstring
  !
  ! vstrlist_split_vstring --
  !   Computes an list of vstrings whose elements are the components in the current string.
  !   Returns a list of vstrings created by splitting string at each character that is in 
  !   the splitChars argument. Each element of the result array will consist of 
  !   the characters from string that lie between instances of the characters in 
  !   splitChars. The number of components is zero if string contains adjacent 
  !   characters in splitChars. If splitChars is an empty string then each character of string 
  !   becomes a separate element of the result list. SplitChars defaults to the 
  !   standard white-space characters (space, newline, carriage return and tab).
  ! Arguments:
  !   this   The current string to process
  !   splitChars The string containing the characters where to split
  !
  function vstrlist_split_vstring ( this , splitChars ) result ( newlist )
    implicit none
    type ( t_vstring ), intent(in) :: this
    type ( t_vstring ), intent(in), optional :: splitChars
    type ( t_vstringlist ) :: newlist
    ! Local variables
    integer :: firstsplitchar
    type ( t_vstring ) :: splitChars_real
    type ( t_vstring ) :: component
    integer :: splitChars_real_length
    integer :: this_length
    integer :: icharacter
    type ( t_vstring ) :: current_char
    integer :: component_length
    !
    ! Process options
    !
    if ( present ( splitChars ) ) then
       call vstring_new ( splitChars_real , splitChars )
    else
       call vstring_new ( splitChars_real , VSTRING_WHITESPACE )
    endif
    !
    ! Initialization
    !
    splitChars_real_length = vstring_length ( splitChars_real )
    this_length = vstring_length ( this )
    !
    ! Process the special case where the splitchars is empty
    !
    if ( splitChars_real_length == 0 ) then
       !
       ! There are no characters in the string to split.
       ! Split at every character.
       !
       call vstrlist_new ( newlist )
       do icharacter = 1 , this_length
          component = vstring_index ( this , icharacter )
          call vstrlist_append ( newlist , component )
          call vstring_free ( component )
       enddo
    else
       call vstrlist_new ( newlist )
       call vstring_new ( component , "" )
       !
       ! Loop over the characters of the current string.
       !
       do icharacter = 1 , this_length
          current_char = vstring_index ( this , icharacter )
          firstsplitchar = vstring_first ( splitChars_real , current_char )
          if ( firstsplitchar /= 0 ) then
             !
             ! Current character is a separator
             !
             call vstrlist_append ( newlist , component )
             call vstring_free ( component )
             call vstring_new ( component , "" )
          else
             call vstring_append ( component , current_char )
          endif
          call vstring_free ( current_char )
       enddo
       ! Store the last component
       component_length = vstring_length ( component )
       if ( component_length > 0 ) then
          call vstrlist_append ( newlist , component )
       endif
       call vstring_free ( component )
    endif
    !
    ! Clean-up
    !
    call vstring_free ( splitChars_real )
  end function vstrlist_split_vstring
  !
  ! vstrlist_split_vstring --
  !   Interface to vstrlist_split_vstring to manage character strings
  !
  function vstrlist_split_charstring ( this , splitChars ) result ( newlist )
    implicit none
    type ( t_vstring ), intent(in) :: this
    character(len=*), intent(in) :: splitChars
    type ( t_vstringlist ) :: newlist
    type ( t_vstring ) :: vsplitChars
    call vstring_new ( vsplitChars , splitChars )
    newlist = vstrlist_split_vstring ( this , vsplitChars )
    call vstring_free ( vsplitChars )
  end function vstrlist_split_charstring
  !
  ! vstrlist_join_vstring --
  !   This command returns the string formed by joining all 
  !   of the elements of list together with joinString separating 
  !   each adjacent pair of elements. 
  !   The joinString argument defaults to a space character.
  ! Arguments:
  !   this : the list of vstrings
  !   joinString : the string which is used to join the elements
  !
  function vstrlist_join_vstring ( this , joinString ) result ( newstring )
    implicit none
    type ( t_vstringlist ), intent(in) :: this
    type ( t_vstring ), intent(in), optional :: joinString
    type ( t_vstring ) :: newstring
    integer :: numberOfComponents
    type ( t_vstring ) :: joinString_real
    integer :: icomponent
    type ( t_vstring ) :: component
    !
    ! Process options
    !
    if ( present ( joinString ) ) then
       call vstring_new ( joinString_real , joinString )
    else
       call vstring_new ( joinString_real , VSTRING_SPACE )
    endif
    !
    ! Initialize
    !
    numberOfComponents = vstrlist_length ( this )
    call vstring_new ( newstring )
    if ( numberOfComponents > 0 ) then
       !
       ! Join the elements
       !
       do icomponent = 1 , numberOfComponents - 1
          component = vstrlist_index ( this , icomponent )
          call vstring_append ( newstring , component )
          call vstring_free ( component )
          call vstring_append ( newstring , joinString_real )
       enddo
       !
       ! Last but not the least
       !
       component = vstrlist_index ( this , numberOfComponents )
       call vstring_append ( newstring , component )
       call vstring_free ( component )
    endif
    !
    ! Clean-up
    !
    call vstring_free ( joinString_real )
  end function vstrlist_join_vstring
  !
  ! vstrlist_join_charstring --
  !   Interface to vstrlist_join to manage char strings
  !
  function vstrlist_join_charstring ( this , joinString ) result ( newstring )
    implicit none
    type ( t_vstringlist ), intent(in) :: this
    character(len=*), intent(in) :: joinString
    type ( t_vstring ) :: newstring
    type ( t_vstring ) :: joinVString
    call vstring_new ( joinVString , joinString )
    newstring = vstrlist_join_vstring ( this , joinVString )
    call vstring_free ( joinVString )
  end function vstrlist_join_charstring
  !
  ! vstrlist_search_vstring --
  !   This command searches the elements of list to see if one of them matches 
  !   pattern. If so, the command returns the index of the first matching element.
  !   If not, the command returns 0.
  ! Arguments
  !   pattern : the pattern against which the items of the list are compared.
  !   first : If the optional argument "first" is provided, then the list is searched 
  !     starting at position first.
  !   notmatch : If the optional argument "notmatch" is provided, this negates the sense 
  !     of the match, returning the index of the first non-matching value in the list.
  !   exact : The list element must contain exactly the same string as pattern.
  ! TODO :
  !    -ascii
  !    -decreasing
  !    -dictionary
  !    -exact
  !    -glob
  !    -increasing
  !    -inline
  !    -integer
  !    -real
  !    -regexp : yes yes yes !!!
  !    -sorted
  !
  function vstrlist_search_vstring ( this , pattern , first , notmatch , &
       exact ) result ( strindex )
    implicit none
    type ( t_vstringlist ), intent(in) :: this
    type ( t_vstring ) , intent(in) :: pattern
    integer , intent(in), optional :: first
    logical , intent(in), optional :: notmatch
    logical , intent(in), optional :: exact
    integer :: strindex
    integer :: length
    integer :: icomponent
    logical :: match
    type ( t_vstring ) :: item
    integer :: first_real
    logical :: notmatch_real
    logical :: exact_real
    !
    ! Process options
    !
    if ( present ( first ) ) then
       first_real = first
    else
       first_real = 1
    endif
    if ( present ( notmatch ) ) then
       notmatch_real = notmatch
    else
       notmatch_real = .false.
    endif
    if ( present ( exact ) ) then
       exact_real = exact
    else
       exact_real = .false.
    endif
    length = vstrlist_length ( this )
    !
    ! By default, no item matches
    !
    strindex = VSTRINGLIST_INDEX_UNKNOWN
    !
    ! Now search the item
    !
    do icomponent = first_real , length
       item = vstrlist_index ( this , icomponent )
       if ( exact_real ) then
          match = vstring_equals ( item , pattern )
       else
          match = vstring_match ( item , pattern )
       endif
       call vstring_free ( item )
       if ( notmatch_real ) then
          if ( .NOT. match ) then
             strindex = icomponent
             exit
          endif
       else
          if ( match ) then
             strindex = icomponent
             exit
          endif
       endif
    enddo
  end function vstrlist_search_vstring
  !
  ! vstrlist_search_charstring --
  !   Interface to vstrlist_search_vstring to manage character string pattern
  !
  function vstrlist_search_charstring ( this , pattern , first , notmatch , &
       exact ) result ( strindex )
    implicit none
    type ( t_vstringlist ), intent(in) :: this
    character ( len = * ) , intent(in) :: pattern
    integer , intent(in), optional :: first
    logical , intent(in), optional :: notmatch
    logical , intent(in), optional :: exact
    integer :: strindex
    type ( t_vstring ) :: vpattern
    call vstring_new ( vpattern , pattern )
    strindex = vstrlist_search_vstring ( this , vpattern , first , notmatch , exact )
    call vstring_free ( vpattern )
  end function vstrlist_search_charstring
  !
  ! vstrlist_lsearch_vstring --
  !   This command searches the elements of list to see if one of them matches 
  !   pattern. If so, the command returns the list of the first matching element.
  !   If not, the command returns 0.
  ! Arguments
  !   pattern : the pattern against which the items of the list are compared.
  !   first : If the optional argument "first" is provided, then the list is searched 
  !     starting at position first.
  !   notmatch : If the optional argument "notmatch" is provided, this negates the sense 
  !     of the match, returning the index of the first non-matching value in the list.
  !   exact : The list element must contain exactly the same string as pattern.
  !   allitems : If provided and true, returns the list of all matching elements.
  ! TODO :
  !    -ascii
  !    -decreasing
  !    -dictionary
  !    -exact
  !    -glob
  !    -increasing
  !    -inline
  !    -integer
  !    -real
  !    -sorted
  !
  function vstrlist_lsearch_vstring ( this , pattern , first , notmatch , &
       exact , allitems ) result ( newlist )
    implicit none
    type ( t_vstringlist ), intent(in) :: this
    type ( t_vstring ) , intent(in) :: pattern
    integer , intent(in), optional :: first
    logical , intent(in), optional :: notmatch
    logical , intent(in), optional :: exact
    logical , intent(in), optional :: allitems
    type ( t_vstringlist ) :: newlist
    integer :: strindex
    type ( t_vstring) :: current
    integer :: strindex_current
    logical :: done
    logical :: allitems_real
    !
    ! Process options
    !
    if ( present ( allitems ) ) then
       allitems_real = allitems
    else
       allitems_real = .false.
    endif
    !
    ! Initialize
    !
    if ( present ( first ) ) then
       strindex_current = first
    else
       strindex_current = 1
    endif
    done = .false.
    call vstrlist_new ( newlist )
    !
    ! Search until no element match
    !
    do while ( .NOT. done )
       strindex = vstrlist_search_vstring ( this , pattern , strindex_current , notmatch , &
            exact )
       if ( strindex == VSTRINGLIST_INDEX_UNKNOWN ) then
          done = .true.
       else
          current = vstrlist_index ( this , strindex )
          call vstrlist_append ( newlist , current )
          call vstring_free ( current )
       endif
       ! Update the first element to look at
       strindex_current = strindex + 1
       if ( .NOT. allitems_real ) then
          done = .true.
       endif
    enddo
  end function vstrlist_lsearch_vstring
  !
  ! vstrlist_lsearch_charstring --
  !   Interface to vstrlist_lsearch_vstring to manage character string pattern
  !
  function vstrlist_lsearch_charstring ( this , pattern , first , notmatch , &
       exact , allitems ) result ( newlist )
    implicit none
    type ( t_vstringlist ), intent(in) :: this
    character ( len = * ) , intent(in) :: pattern
    integer , intent(in), optional :: first
    logical , intent(in), optional :: notmatch
    logical , intent(in), optional :: exact
    logical , intent(in), optional :: allitems
    type ( t_vstringlist ) :: newlist
    type ( t_vstring) :: vpattern
    call vstring_new ( vpattern , pattern )
    newlist = vstrlist_lsearch_vstring ( this , vpattern , first , notmatch , &
         exact , allitems )
    call vstring_free ( vpattern )
  end function vstrlist_lsearch_charstring
  !
  ! vstrlist_sort --
  !   Sort the elements of a list.
  ! TODO : fill that missing piece
  !

  !
  ! vstrlist_check_index --
  !   Check that the given integer index exist in the current list.
  !   Generates an error if not.
  !   If no error occurs, set the status to azero value.
  !   If an error occurs, set the status to a non-zero value.
  !
  subroutine vstrlist_check_index ( this , icomponent , status )
    type ( t_vstringlist ) , intent(in) :: this
    integer, intent(in) :: icomponent
    integer , intent(out) :: status
    integer :: length
    character ( len = 400 ) :: message
    status = VSTRINGLIST_ERROR_OK
    if ( icomponent < 1 ) then
       status = VSTRINGLIST_ERROR_INDEXWRONG
       write ( message , * ) "The given index ", icomponent , " is lower than 1."
       call vstring_error ( this , message )
    endif
    length = vstrlist_length ( this )
    if ( icomponent > length ) then
       status = VSTRINGLIST_ERROR_INDEXWRONG
       write ( message , * ) "The given index ", icomponent , &
            " is greater than the length of the list : ", length
       call vstring_error ( this , message )
    endif
  end subroutine vstrlist_check_index
  !
  ! vstrlist_error --
  !   Generates an error for the string list.
  !
  subroutine vstring_error ( this , message )
    implicit none
    type ( t_vstringlist ) , intent(in) :: this
    character ( len = * ), intent (in) :: message
    integer :: length
    logical :: exists
    write ( * , * ) "Internal error in m_vstringlist"
    length =  vstrlist_length ( this )
    exists = vstrlist_exists ( this )
    if ( exists ) then
       write ( * , * ) "Length:" , length
    endif
#ifdef _VSTRINGLIST_ALLOCATABLE
    write ( * , * ) "Version : allocatable"
#endif
#ifdef _VSTRINGLIST_POINTERS
    write ( * , * ) "Version : pointer"
#endif
    write ( * , * ) "Message :", trim( message )
    if ( vstringlist_stoponerror ) then
       STOP
    endif
  end subroutine vstring_error
  ! 
  ! vstringlist_set_stoponerror --
  !   Configure the behaviour of the componenent whenever an 
  !   error is met.
  !   If stoponerror is true, then the execution stops if an error is encountered.
  !   If stoponerror is false, then the execution continues if an error is encountered.
  !   In both cases, a message is displayed on standard output.
  ! 
  subroutine vstringlist_set_stoponerror ( stoponerror )
    logical , intent(in) :: stoponerror
    vstringlist_stoponerror = stoponerror
    call vstring_set_stoponerror ( stoponerror )
  end subroutine vstringlist_set_stoponerror
end module m_vstringlist

