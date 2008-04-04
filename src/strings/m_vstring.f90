!
! m_vstring --
!   This module provides services to manage strings of dynamic length.
!
! ******************************************************************************
! *                                                                            *
! * iso_varying_string.f90                                                     *
! *                                                                            *
! * Copyright (c) 2003, Rich Townsend <rhdt@bartol.udel.edu>                   *
! * All rights reserved.                                                       *
! *                                                                            *
! * Redistribution and use in source and binary forms, with or without         *
! * modification, are permitted provided that the following conditions are     *
! * met:                                                                       *
! *                                                                            *
! *  * Redistributions of source code must retain the above copyright notice,  *
! *    this list of conditions and the following disclaimer.                   *
! *  * Redistributions in binary form must reproduce the above copyright       *
! *    notice, this list of conditions and the following disclaimer in the     *
! *    documentation and/or other materials provided with the distribution.    *
! *                                                                            *
! * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS    *
! * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,  *
! * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR     *
! * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR           *
! * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,      *
! * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,        *
! * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR         *
! * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF     *
! * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING       *
! * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS         *
! * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.               *
! *                                                                            *
! ******************************************************************************
!
! Copyright (c) 2008 Michaël Baudin
!
! $Id$
!
! Thanks    : Lawrie Schonfelder (bugfixes and design pointers), Walt Brainerd
!             (conversion to F).
! Note  : The current module does NOT conform to the API
!         specified in ISO/IEC 1539-2:2000 (varying-length strings for
!         Fortran 95).
!
module m_vstring
  implicit none
  private
  !
  ! Type definitions
  !
  ! Choose your dynamic string system between _VSTRING_ALLOCATABLE , _VSTRING_POINTERS
  ! Allocatable arrays should be used by default.
  ! But for compatibility of older fortran 90 compilers, pointers are also available.
  ! These are the recommended settings :
  ! _VSTRING_ALLOCATABLE (default) : gfortran, g95
  ! _VSTRING_POINTERS : Intel Fortran 8.0
  !
#ifndef _VSTRING_POINTERS
#ifndef _VSTRING_ALLOCATABLE
#define _VSTRING_ALLOCATABLE
#endif
#endif
  type, public :: t_vstring
     private
#ifdef _VSTRING_ALLOCATABLE
     character(LEN=1), dimension(:), allocatable :: chars
     !character, allocatable :: chars(:)
#endif
#ifdef _VSTRING_POINTERS
     !character(LEN=1), dimension(:), pointer :: chars => NULL()
     character, pointer :: chars(:) => NULL()
#endif
  end type t_vstring
  !
  ! vstring_new --
  ! Constructor
  !
  interface vstring_new
     module procedure vstring_new_from_charstring
     module procedure vstring_new_from_vstring
     module procedure vstring_new_from_chararray
     module procedure vstring_new_from_integer
  end interface vstring_new
  !
  ! Methods
  !
  !
  ! Public methods
  !
  interface vstring_tocharstring
     module procedure vstring_tocharstring_fixed
     module procedure vstring_tocharstring_auto
  end interface vstring_tocharstring
  public :: vstring_achar
  public :: vstring_adjustl
  public :: vstring_adjustr
  public :: vstring_allocated
  public :: vstring_append
  public :: vstring_char
  public :: vstring_charindex
  public :: vstring_compare
  public :: vstring_concat
  public :: vstring_equals
  public :: vstring_first
  public :: vstring_free
  public :: vstring_iachar
  public :: vstring_ichar
  public :: vstring_index
  public :: vstring_last
  public :: vstring_length
  public :: vstring_map
  public :: vstring_match
  public :: vstring_new
  public :: vstring_random
  public :: vstring_range
  public :: vstring_reference_get
  public :: vstring_replace
  public :: vstring_reverse
  public :: vstring_scan
  public :: vstring_split
  public :: vstring_tocharstring
  public :: vstring_tolower
  public :: vstring_totitle
  public :: vstring_toupper
  public :: vstring_trim
  public :: vstring_trimleft
  public :: vstring_trimright
  public :: vstring_verify
  !
  ! Constants
  !
  integer, parameter, public :: VSTRING_COMPARE_LOWER = -1
  integer, parameter, public :: VSTRING_COMPARE_EQUAL = 0
  integer, parameter, public :: VSTRING_COMPARE_GREATER = 1
  integer, parameter, public :: VSTRING_INDEX_UNKNOWN = 0
  character(len=*), parameter, private :: VSTRING_UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  character(len=*), parameter, private :: VSTRING_LOWER = "abcdefghijklmnopqrstuvwxyz"
  character(len=*), parameter, private :: VSTRING_CHARACTERSET = "abcdefghijklmnopqrstuvwxyz0123456789"
  character(len=*), parameter, private :: VSTRING_SPACE           = achar(32)
  ! Ascii char #10 -> corresponds to \n : line_feed
  character(len=*), parameter, private :: VSTRING_NEWLINE         = achar(10)
  ! Ascii char #13 -> corresponds to \r : carriage return
  character(len=*), parameter, private :: VSTRING_CARRIAGE_RETURN = achar(13)
  ! Ascii char #9 -> corresponds to \t : tab
  character(len=*), parameter, private :: VSTRING_TAB             = achar(9)
  character(len=*), parameter, private :: VSTRING_WHITESPACE = VSTRING_SPACE//VSTRING_NEWLINE//VSTRING_CARRIAGE_RETURN//VSTRING_TAB
  character(len=*), parameter, private :: VSTRING_DIGITS = "0123456789"
  character(len=*), parameter, private :: VSTRING_HEXDIGITS = "0123456789abcdefABCDEF"
  !
  ! Static parameters
  !
  logical, save :: random_process_initialize = .false.
  ! Total number of currently available (allocated) strings.
  ! Note :
  ! This is mainly for debugging purposes of the vstring module itself or client algorithms.
  ! It allows to check the consistency of vstring_new/vstring_free statements.
  integer, save :: vstring_number_of_strings = 0
contains
  !
  ! vstring_new_from_charstring --
  !   Constructor based on a character(len=*)
  !
  subroutine vstring_new_from_charstring ( this , char_string )
    type(t_vstring), intent(inout) :: this
    character(LEN=*), intent(in)    :: char_string
    character ( len = 200) :: message
    integer :: length
    integer :: icharacter
    logical :: isallocated
    integer :: this_length
    !
    ! Check that the data is empty
    !
    isallocated = vstring_allocated ( this )
    if ( isallocated ) then
       this_length = vstring_length(this)
       write ( message , * ) "Object is allready associated with size ", this_length
       call vstring_error ( this , message , "vstring_new_from_charstring" )
    endif
    length = LEN ( char_string )
    allocate ( this % chars(length))
    do icharacter = 1, length
       this % chars(icharacter) = char_string (icharacter:icharacter)
    enddo
    !
    ! Update the counter of strings
    !
    call vstring_reference_add ()
  end subroutine vstring_new_from_charstring
  !
  ! vstring_new_from_vstring --
  !   Constructor based on a vstring
  !
  subroutine vstring_new_from_vstring ( this , vstring )
    type(t_vstring), intent(inout) :: this
    type(t_vstring), intent(in) :: vstring
    integer :: length
    character ( len = 200) :: message
    integer :: icharacter
    integer :: this_length
    call vstring_check_string ( vstring , "vstring_new_from_vstring" )
    !
    ! Check that the data is empty
    !
    if ( vstring_allocated ( this ) ) then
       this_length = vstring_length(this)
       write ( message , * ) "Object is allready associated with size ", this_length , &
            " in vstring_new_from_charstring"
       call vstring_error ( this , message )
    endif
    length = vstring_length ( vstring )
    allocate ( this % chars ( length ) )
    do icharacter = 1 , length
       this % chars(icharacter) = vstring % chars (icharacter)
    enddo
    !
    ! Update the counter of strings
    !
    call vstring_reference_add ()
  end subroutine vstring_new_from_vstring
  !
  ! vstring_new_from_chararray --
  !   Constructor based on an array of characters
  !
  subroutine vstring_new_from_chararray ( this , chararray )
    type(t_vstring), intent(inout) :: this
    character(len=1), dimension(:), intent(in) :: chararray
    integer :: length
    character ( len = 300 ) :: message
    integer :: icharacter
    integer :: this_length
    !
    ! Check that the data is empty
    !
    if ( vstring_allocated ( this ) ) then
       this_length = vstring_length(this)
       write ( message , * ) "Object is allready associated with size ", this_length , &
            " in vstring_new_from_chararray"
       call vstring_error ( this , message )
    endif
    length = size ( chararray )
    allocate ( this % chars ( length ) )
    do icharacter = 1, length
       this % chars(icharacter) = chararray (icharacter)(1:1)
    enddo
    !
    ! Update the counter of strings
    !
    call vstring_reference_add ()
  end subroutine vstring_new_from_chararray
  !
  ! vstring_new_from_integer --
  !   Repeat the string ncount times and concatenate the result to create the new string.
  !   If not given, the default string is the blank space.
  ! Note :
  !   This can be considered as an implementation of "vstring_repeat".
  !
  subroutine vstring_new_from_integer ( this , ncount , string )
    type(t_vstring), intent(inout) :: this
    integer, intent(in) :: ncount
    type(t_vstring), intent(in), optional :: string
    integer :: length
    character ( len = 300 ) :: message
    type(t_vstring) :: string_real
    integer :: icount
    integer :: string_length
    integer :: first
    integer :: last
    integer :: this_length
    !
    ! Check that the data is empty
    !
    if ( vstring_allocated ( this ) ) then
       this_length = vstring_length(this)
       write ( message , * ) "Object is allready associated with size ", this_length , &
            " in vstring_new_from_chararray"
       call vstring_error ( this , message )
    endif
    !
    ! Check the value of the integer
    !
    if ( ncount < 0 ) then
       write ( message , * ) "The given number of counts ", ncount , " is inconsistent ", &
            " in vstring_new_from_chararray"
       call vstring_error ( this , message )
    endif
    !
    ! Process options
    !
    if (present ( string ) ) then
       call vstring_new ( string_real , string )
    else
       call vstring_new ( string_real , VSTRING_SPACE )
    endif
    !
    ! Create the object
    !
    string_length = vstring_length ( string_real )
    length = ncount * string_length
    allocate ( this % chars ( length ) )
    do icount = 1, ncount
       first = string_length * ( icount - 1 ) + 1
       last = string_length * icount
       this % chars( first : last ) = string_real % chars ( 1 : string_length )
    enddo
    !
    ! Cleanup
    !
    call vstring_free ( string_real )
    !
    ! Update the counter of strings
    !
    call vstring_reference_add ()
  end subroutine vstring_new_from_integer
  !
  ! vstring_free --
  !   Destructor.
  ! NOTE :
  !   The use of the destructor is OPTIONAL.
  !   See the thread " New ISO_VARYING_STRING implementation (without memory leaks)" on comp.lang.fortran :
  !   "On most systems, memory is memory :-).  However, there is a
  !   difference between how ALLOCATABLE variables and POINTER
  !   variables are handled.  ALLOCATABLE variables are always
  !   deallocated automatically when thay go out of scope (unless
  !   they have the SAVE attribute).  POINTER variables usually
  !   are not.  The reason is that the program may have associated
  !   additional pointers, that aren't going out of scope, with the
  !   same target as the one that is."
  !
  subroutine vstring_free ( this )
    type(t_vstring), intent(inout) :: this
    logical :: string_allocated
    character ( len = 300 ) :: message
    integer :: status
    string_allocated = vstring_allocated ( this )
    if ( string_allocated ) then
       deallocate ( this % chars , stat=status )
       if ( status /= 0 ) then
          write ( message , * ) "There was an error while deallocating the string."
          call vstring_error ( this , message , "vstring_free" )
       endif
#ifdef _VSTRING_POINTERS
       nullify ( this % chars )
#endif
       !
       ! Update the counter of strings
       !
       call vstring_reference_remove ()
    else
       write ( message , * ) "The current varying string is not allocated ", &
            " in vstring_free"
       call vstring_error ( this , message , "vstring_free" )
    endif
  end subroutine vstring_free
  !
  ! vstring_reference_add --
  !   Static method.
  !   Increase the counter of currently referenced strings.
  !
  subroutine vstring_reference_add ( )
    implicit none
    vstring_number_of_strings = vstring_number_of_strings + 1
  end subroutine vstring_reference_add
  !
  ! vstring_reference_remove --
  !   Static method.
  !   Decrease the counter of currently referenced strings.
  !
  subroutine vstring_reference_remove ( )
    implicit none
    vstring_number_of_strings = vstring_number_of_strings - 1
  end subroutine vstring_reference_remove
  !
  ! vstring_reference_get --
  !   Static method.
  !   Decrease the counter of currently referenced strings.
  !
  integer function vstring_reference_get ( )
    implicit none
    vstring_reference_get = vstring_number_of_strings
  end function vstring_reference_get
  !
  ! vstring_allocated --
  !   Returns .true. if the string is allocated.
  !
  pure logical function vstring_allocated ( this )
    type(t_vstring), intent(in) :: this
#ifdef _VSTRING_ALLOCATABLE
    if ( allocated ( this%chars) ) then
       vstring_allocated = .true.
    else
       vstring_allocated = .false.
    endif
#endif
#ifdef _VSTRING_POINTERS
    if ( associated ( this%chars) ) then
       vstring_allocated = .true.
    else
       vstring_allocated = .false.
    endif
#endif
  end function vstring_allocated
  !
  ! vstring_equals --
  !   Perform a character-by-character comparison of strings string1 and string2.
  !   Returns true if this and string2 are identical, or .false when not.
  !   If nocase is set to true, the case of the characters is not taken into account.
  !   The default behaviour is to take into account for case of characters.
  !   If length is specified, then only the first length characters are used in the comparison.
  !
  function vstring_equals ( this , string_b , nocase , length ) result (op_eq)
    type(t_vstring), intent(in) :: this
    type(t_vstring), intent(in) :: string_b
    logical , intent (in), optional :: nocase
    integer , intent ( in ), optional :: length
    logical                          :: op_eq
    logical :: nocase_real
    integer :: length_a
    integer :: length_b
    integer :: icharacter
    type(t_vstring) :: char1
    type(t_vstring) :: char2
    type(t_vstring) :: char1_case
    type(t_vstring) :: char2_case
    integer :: length_real
    integer :: length_min
    integer :: last
    character ( len = 300 ) :: message
    call vstring_check_string ( this , "vstring_equals" )
    call vstring_check_string ( string_b , "vstring_equals" )
    !
    ! Process options
    !
    if ( present ( nocase ) ) then
       nocase_real = nocase
    else
       nocase_real = .false.
    endif
    !
    ! If the length parameter is here, it must be consistent with both strings lengths
    !
    length_a = vstring_length ( this )
    length_b = vstring_length ( string_b )
    length_min = min ( length_a , length_b )
    if ( present ( length ) ) then
       if ( length > length_min ) then
          write ( message , * ) "The given number of characters to compare ", length ,&
               " does not match the number of characters present in both strings : ", length_min, &
               " in vstring_equals."
          call vstring_error ( this , message )
       endif
       length_real = length
    else
       length_real = vstring_length ( this )
    endif
    !
    ! Compare lengths, if the length optional parameter is not here.
    !
    op_eq = .true.
    if ( .NOT. present ( length ) ) then
       if (length_a/=length_b) then
          op_eq = .false.
       endif
    endif
    !
    ! Compare characters.
    !
    if ( op_eq ) then
       last = min ( length_real , length_min )
       do icharacter = 1 , last
          !
          ! Compute the character at index #icharacter, with modified case if necessary.
          !
          char1 = vstring_index ( this , icharacter )
          char2 = vstring_index ( string_b , icharacter )
          if ( nocase_real ) then
             char1_case = vstring_tolower ( char1 )
             char2_case = vstring_tolower ( char2 )
          else
             call vstring_new ( char1_case , char1 )
             call vstring_new ( char2_case , char2 )
          endif
          !
          ! Make the comparison
          !
          if ( char1_case % chars ( 1 )/=char2_case % chars ( 1 ) ) then
             op_eq = .false.
          endif
          call vstring_free ( char1 )
          call vstring_free ( char2 )
          call vstring_free ( char1_case )
          call vstring_free ( char2_case )
          if (.NOT.op_eq) then
             exit
          endif
       enddo
    endif
  end function vstring_equals
  !
  ! vstring_tocharstring_fixed --
  !   Convert a varying string into a character string
  !   (fixed length)
  !
  subroutine vstring_tocharstring_fixed ( this , length , char_string )
    type(t_vstring), intent(in) :: this
    integer, intent(in)              :: length
    character ( LEN = length ) , intent(out) :: char_string
    integer :: length_this
    character ( len = 300 ) :: message
    integer :: icharacter
    length_this = vstring_length ( this )
    if ( length < length_this ) then
       write ( message , * ) "The given character string is of length ", length , &
            " which smaller than the number of characters in the varying string ", length_this
       call vstring_error ( this , message )
    endif
    do icharacter = 1, length_this
       char_string ( icharacter : icharacter ) = this % chars ( icharacter )
    end do
    !
    ! Pad with white spaces
    !
    do icharacter = length_this + 1 , length
       char_string ( icharacter : icharacter ) = VSTRING_SPACE
    end do
  end subroutine vstring_tocharstring_fixed
  !
  ! vstring_tocharstring_auto --
  !   Convert a varying string into a character string
  !   (automatic length)
  !
  subroutine vstring_tocharstring_auto ( this , char_string )
    type(t_vstring), intent(in) :: this
    character ( LEN = * ) , intent(out) :: char_string
    integer :: length_this
    integer :: icharacter
    length_this = vstring_length ( this )
    do icharacter = 1, length_this
       char_string ( icharacter : icharacter ) = this % chars ( icharacter )
    end do
  end subroutine vstring_tocharstring_auto
  !
  ! vstring_length --
  !   Compute the length of a varying string
  !
  pure function vstring_length ( this ) result (length)
    type(t_vstring), intent(in) :: this
    integer                          :: length
    logical :: string_allocated
    string_allocated = vstring_allocated ( this )
    if ( string_allocated ) then
       length = SIZE(this%chars)
    else
       length = 0
       ! One cannot use the error management system provided by vstring_error
       ! because it is convenient that vstring_length is pure,
       ! to set the dimension of automatic-length character strings.
       !write ( message , * ) "Object is not created in vstring_length"
       !call vstring_error ( this , message )
    endif
  end function vstring_length
  !
  ! vstring_concat --
  !   Returns a new string made by the concatenation of two varying strings.
  !
  function vstring_concat ( this , string_b ) result (concat_string)
    type(t_vstring), intent(in) :: this
    type(t_vstring), intent(in) :: string_b
    type(t_vstring)             :: concat_string
    integer                          :: len_string_a
    integer :: concat_length
    call vstring_check_string ( this , "vstring_concat" )
    call vstring_check_string ( string_b , "vstring_concat" )
    len_string_a = vstring_length(this)
    concat_length = len_string_a+vstring_length(string_b)
    call vstring_new ( concat_string , concat_length )
    concat_string % chars(:len_string_a) = this%chars
    concat_string % chars(len_string_a+1:) = string_b%chars
  end function vstring_concat
  !
  ! vstring_compare --
  !   Perform a character-by-character comparison of strings string1 and string2.
  !   Returns -1, 0, or 1, depending on whether string1 is lexicographically less
  !   than, equal to, or greater than string2.
  !   If -length is specified, then only the first length characters are used in the comparison.
  !
  function vstring_compare ( this , string_b , nocase , length ) result ( compare )
    type(t_vstring), intent(in) :: this
    type(t_vstring), intent(in) :: string_b
    logical , intent (in), optional :: nocase
    integer , intent ( in ), optional :: length
    integer                     :: compare
    integer :: common_length
    integer :: length_this , length_b
    integer :: icharacter
    logical :: comparison_done
    logical :: nocase_real
    type(t_vstring) :: char1
    type(t_vstring) :: char2
    type(t_vstring) :: char1_case
    type(t_vstring) :: char2_case
    character (len=300) :: message
    !
    ! Process options
    !
    if ( present ( nocase ) ) then
       nocase_real = nocase
    else
       nocase_real = .false.
    endif
    call vstring_check_string ( this , "vstring_compare" )
    call vstring_check_string ( string_b , "vstring_compare" )
    !
    ! Initialize
    !
    length_this = vstring_length ( this )
    length_b = vstring_length ( string_b )
    common_length = min ( length_this , length_b )
    comparison_done = .false.
    !
    ! If the length optional parameter is given, reduce the number of characters which are compared.
    !
    if ( present ( length ) ) then
       if ( length > common_length ) then
          write ( message , * ) "The given number of characters to compare ", length ,&
               " does not match the number of characters present in both strings : ", common_length , &
               " in vstring_equals."
          call vstring_error ( this , message )
       endif
       common_length = length
    endif    !
    ! Compare character by character until there is one difference
    !
    do icharacter = 1 , common_length
       !
       ! Compute the character at index #icharacter, with modified case if necessary.
       !
       char1 = vstring_index ( this , icharacter )
       char2 = vstring_index ( string_b , icharacter )
       if ( nocase_real ) then
          char1_case = vstring_tolower ( char1 )
          char2_case = vstring_tolower ( char2 )
       else
          call vstring_new ( char1_case , char1 )
          call vstring_new ( char2_case , char2 )
       endif
       !
       ! Make the comparison of the modified characters
       !
       if ( char1_case % chars ( 1 ) < char2_case % chars ( 1 ) ) then
          comparison_done = .true.
          compare = VSTRING_COMPARE_LOWER
       elseif ( char1_case % chars ( 1 ) > char2_case % chars ( 1 ) ) then
          comparison_done = .true.
          compare = VSTRING_COMPARE_GREATER
       endif
       call vstring_free ( char1 )
       call vstring_free ( char2 )
       call vstring_free ( char1_case )
       call vstring_free ( char2_case )
       !
       ! Note : the "exit" is done afterwards, in order to let the system free the characters.
       !
       if ( comparison_done ) then
          exit
       endif
    enddo
    !
    ! If the common part is the same, compare the lengths
    !
    if ( .NOT. comparison_done ) then
       if ( present ( length ) ) then
          ! If the length argument is provided, then the previous algorithm has
          ! proved that all characters from 1 to length are equal.
          compare = VSTRING_COMPARE_EQUAL
       else
          if ( length_this == length_b ) then
             compare = VSTRING_COMPARE_EQUAL
          elseif ( length_this > length_b ) then
             compare = VSTRING_COMPARE_GREATER
          else
             compare = VSTRING_COMPARE_LOWER
          endif
       endif
    endif
  end function vstring_compare
  !
  ! vstring_trim --
  !   Returns a new string except that any leading or trailing characters from the set given by chars are removed.
  !   If chars is not specified then white space is removed (spaces, tabs, newlines, and carriage returns).
  !
  function vstring_trim ( this , chars ) result ( trim_string )
    type(t_vstring), intent(in) :: this
    type(t_vstring), intent(in), optional :: chars
    type(t_vstring)             :: trim_string
    type(t_vstring)             :: chars_real
    type(t_vstring) :: trimmedLeft
    call vstring_check_string ( this , "vstring_trim" )
    if (present ( chars )) then
       call vstring_check_string ( chars , "vstring_trim" )
       call vstring_new ( chars_real , chars )
    else
       call vstring_new ( chars_real , VSTRING_WHITESPACE )
    endif
    !
    ! 1. Trim left
    !
    trimmedLeft = vstring_trimleft ( this , chars_real )
    !
    ! 2. Trim right
    !
    trim_string = vstring_trimright ( trimmedLeft , chars_real )
    !
    ! 3. Cleanup
    !
    call vstring_free ( chars_real )
    call vstring_free ( trimmedLeft )
  end function vstring_trim
  !
  ! vstring_trimleft --
  !   Returns a new string except that any leading characters from the set given by chars are removed.
  !   If chars is not specified then white space is removed (spaces, tabs, newlines, and carriage returns).
  !
  function vstring_trimleft ( this , chars ) result ( trim_string )
    type(t_vstring), intent(in) :: this
    type(t_vstring), intent(in), optional :: chars
    type(t_vstring)             :: trim_string
    type(t_vstring)             :: chars_real
    integer :: length
    integer :: icharacter
    integer :: searched_index
    type(t_vstring) :: current_char
    integer :: first
    logical :: first_found
    call vstring_check_string ( this , "vstring_trimleft" )
    if (present ( chars )) then
       call vstring_check_string ( chars , "vstring_trimleft" )
       call vstring_new ( chars_real , chars )
    else
       call vstring_new ( chars_real , VSTRING_WHITESPACE )
    endif
    length = vstring_length ( this )
    !
    ! 1. Compute the first character index which is not in the set of characters to remove
    !
    first_found = .false.
    do icharacter = 1 , length
       current_char = vstring_index ( this , icharacter )
       searched_index = vstring_first ( chars_real , current_char )
       if ( searched_index == VSTRING_INDEX_UNKNOWN ) then
          first = icharacter
          first_found = .true.
       endif
       call vstring_free ( current_char )
       if ( first_found ) then
          exit
       endif
    enddo
    !
    ! 2. If there are characters not to remove, the result is the string range from first to length.
    ! If all the characters of the current string are to remove, create an empty string.
    !
    if ( first_found ) then
       trim_string = vstring_range ( this , first , length )
    else
       call vstring_new ( trim_string , "" )
    endif
    !
    ! 3. Cleanup
    !
    call vstring_free ( chars_real )
  end function vstring_trimleft
  !
  ! vstring_trimright --
  !   Returns a value equal to string except that any trailing characters from the set given by chars are removed.
  !   If chars is not specified then white space is removed (spaces, tabs, newlines, and carriage returns).
  !
  function vstring_trimright ( this , chars ) result ( trim_string )
    type(t_vstring), intent(in) :: this
    type(t_vstring), intent(in), optional :: chars
    type(t_vstring)             :: trim_string
    type(t_vstring)             :: chars_real
    integer :: length
    integer :: icharacter
    integer :: searched_index
    type(t_vstring) :: current_char
    integer :: last
    logical :: last_found
    call vstring_check_string ( this , "vstring_trimright" )
    if (present ( chars )) then
       call vstring_check_string ( chars , "vstring_trimright" )
       call vstring_new ( chars_real , chars )
    else
       call vstring_new ( chars_real , VSTRING_WHITESPACE )
    endif
    length = vstring_length ( this )
    !
    ! 1. Compute the last character index which is not in the set of characters to remove
    !
    last_found = .false.
    do icharacter = length , 1 , -1
       current_char = vstring_index ( this , icharacter )
       searched_index = vstring_last ( chars_real , current_char )
       if ( searched_index == 0 ) then
          last = icharacter
          last_found = .true.
       endif
       call vstring_free ( current_char )
       if ( last_found ) then
          exit
       endif
    enddo
    !
    ! 2. If there are characters not to remove, the result is the string range from 1 to last.
    ! If all the characters of the current string are to remove, create an empty string.
    !
    if ( last_found ) then
       trim_string = vstring_range ( this , 1 , last )
    else
       call vstring_new ( trim_string , "" )
    endif
    !
    ! 3. Cleanup
    !
    call vstring_free ( chars_real )
  end function vstring_trimright
  !
  ! vstring_first --
  !   Search in the current string for a sequence of characters that exactly match the characters in string2.
  !   If found, return the index of the first character in the first such match within the current string.
  !   If not found, return 0.
  !   If first is specified, then the search is constrained to start with the character 
  !   in the current string specified by the index.
  !
  function vstring_first ( this , string2 , first ) result ( firstIndex )
    type(t_vstring), intent(in)   :: this
    type(t_vstring), intent(in)   :: string2
    integer, intent(in), optional :: first
    integer                       :: firstIndex
    integer :: startIndex
    integer :: icharacter
    integer :: length
    integer :: length_searched
    integer :: endIndex
    type(t_vstring)   :: substring
    logical :: equals
    call vstring_check_string ( this , "vstring_first" )
    call vstring_check_string ( string2 , "vstring_first" )
    if (present( first )) then
       call vstring_check_index ( this , first )
       startIndex = first
    else
       startIndex = 1
    endif
    length = vstring_length ( this )
    length_searched = vstring_length ( string2 )
    firstIndex = VSTRING_INDEX_UNKNOWN
    do icharacter = startIndex , length
       !
       ! The length of the string to search for is 0, which never matches.
       !
       if ( length_searched == 0 ) then
          exit
       endif
       endIndex = icharacter + length_searched - 1
       ! The number of characters to search for is greater than the number
       ! of characters which are left to compare.
       if ( endIndex > length ) then
          exit
       endif
       substring = vstring_range ( this , icharacter , endIndex )
       equals = vstring_equals ( substring , string2 )
       call vstring_free ( substring )
       if ( equals ) then
          firstIndex = icharacter
          exit
       endif
    enddo
  end function vstring_first
  !
  ! vstring_last --
  !   Search in the current string for a sequence of characters that exactly match the characters in string2.
  !   If found, return the index of the last character in the first such match within the current string.
  !   If not found, return 0.
  !   If last is specified, then the search is constrained to start with the character in the current 
  !   string specified by the index.
  !
  function vstring_last ( this , string2 , last ) result ( first )
    type(t_vstring), intent(in)   :: this
    type(t_vstring), intent(in)   :: string2
    integer, intent(in), optional :: last
    integer                       :: first
    integer :: lastIndex
    integer :: icharacter
    integer :: length
    integer :: length_searched
    integer :: endIndex
    type(t_vstring)   :: substring
    logical :: equals
    call vstring_check_string ( this , "vstring_last" )
    call vstring_check_string ( string2  , "vstring_last" )
    length = vstring_length ( this )
    if (present( last )) then
       call vstring_check_index ( this , last )
       lastIndex = last
    else
       lastIndex = length
    endif
    length_searched = vstring_length ( string2 )
    first = VSTRING_INDEX_UNKNOWN
    do icharacter = lastIndex , 1 , -1
       !
       ! The length of the string to search for is 0, which never matches.
       !
       if ( length_searched == 0 ) then
          exit
       endif
       endIndex = icharacter + length_searched - 1
       ! The number of characters to search for is greater than the number
       ! of characters which are left to compare.
       if ( endIndex > length ) then
          exit
       endif
       substring = vstring_range ( this , icharacter , endIndex )
       equals = vstring_equals ( substring , string2 )
       call vstring_free ( substring )
       if ( equals ) then
          first = icharacter
          exit
       endif
    enddo
  end function vstring_last
  !
  ! vstring_range --
  !   Returns a range of consecutive characters from string,
  !   starting with the character whose index is first and ending with the character whose
  !   index is last. An index of 1 refers to the first character of the string.
  !   If first is less than 1 then an error is generated.
  !   If last is greater than or equal to the length of the string then an error is generated.
  !   If first is greater than last then an error is generated.
  ! TODO : first and last may be specified as for the index method.
  !
  function vstring_range ( this , first , last ) result ( string_range )
    type(t_vstring), intent(in) :: this
    integer, intent(in)         :: first , last
    type(t_vstring)             :: string_range
    character ( len = 200) :: message
    call vstring_check_string ( this , "vstring_range" )
    call vstring_check_index ( this , first , "vstring_range" )
    call vstring_check_index ( this , last , "vstring_range" )
    if ( first > last ) then
       write ( message , * ) "First index ", first , " is greater than last ", last, &
            " in vstring_range"
       call vstring_error ( this , message )
    endif
    call vstring_new ( string_range , this % chars ( first : last ) )
  end function vstring_range
  !
  ! vstring_index --
  !   Returns the charIndex'th character of the string argument.
  !   A charIndex of 1 corresponds to the first character of the string.
  !   If charIndex is less than 1 or greater than or equal to the length of the string
  !   then an error is generated.
  ! TODO : index can be given as the string "end" and corresponds to the last char of the string.
  !
  function vstring_index ( this , charIndex ) result ( string_index )
    type(t_vstring), intent(in) :: this
    integer, intent(in)         :: charIndex
    type(t_vstring)             :: string_index
    call vstring_check_string ( this , "vstring_index" )
    call vstring_check_index ( this , charIndex , "vstring_index" )
    call vstring_new ( string_index , this % chars ( charIndex : charIndex ) )
  end function vstring_index
  !
  ! vstring_toupper --
  !   Returns a value equal to string except that all lower (or title) case letters have been 
  !   converted to upper case. If first is specified, it refers to the first char index in the string 
  !   to start modifying. If last is specified, it refers to the char index in the string to stop 
  !   at (inclusive).
  !
  function vstring_toupper ( this , first , last ) result ( new_upper )
    type(t_vstring), intent(in) :: this
    integer , intent ( in ), optional :: first
    integer , intent ( in ), optional :: last
    type(t_vstring) :: new_upper
    integer :: first_real
    integer :: last_real
    integer :: icharacter
    integer :: firstindex
    !
    ! Get options
    !
    call vstring_check_string ( this , "vstring_toupper" )
    if (present( first )) then
       call vstring_check_index ( this , first , "vstring_toupper" )
       first_real = first
    else
       first_real = 1
    endif
    if (present( last )) then
       call vstring_check_index ( this , last , "vstring_toupper" )
       last_real = last
    else
       last_real = vstring_length ( this )
    endif
    !
    ! Compute the upper case string.
    !
    call vstring_new ( new_upper , this )
    do icharacter = first_real , last_real
       firstindex = index( VSTRING_LOWER , this % chars ( icharacter)  )
       if ( firstindex > 0 ) then
          new_upper % chars ( icharacter ) = VSTRING_UPPER ( firstindex : firstindex )
       endif

    enddo
  end function vstring_toupper
  !
  ! vstring_tolower --
  !   Returns a value equal to string except that all upper (or title) case letters have been 
  !   converted to lower case. If first is specified, it refers to the first char index in the string 
  !   to start modifying. If last is specified, it refers to the char index in the string to stop 
  !   at (inclusive).
  !
  function vstring_tolower ( this , first , last ) result ( new_lower )
    type(t_vstring), intent(in) :: this
    integer , intent ( in ), optional :: first
    integer , intent ( in ), optional :: last
    type(t_vstring) :: new_lower
    integer :: first_real
    integer :: last_real
    integer :: icharacter
    integer :: firstindex
    integer :: length
    !
    ! Get options
    !
    call vstring_check_string ( this , "vstring_tolower" )
    length = vstring_length ( this )
    if (present( first )) then
       call vstring_check_index ( this , first , "vstring_tolower" )
       first_real = first
    else
       first_real = 1
    endif
    if (present( last )) then
       call vstring_check_index ( this , last , "vstring_tolower" )
       last_real = last
    else
       last_real = length
    endif
    !
    ! Compute the lower case string.
    !
    call vstring_new ( new_lower , this )
    do icharacter = first_real , last_real
       firstindex = index( VSTRING_UPPER , this % chars ( icharacter)  )
       if ( firstindex > 0 ) then
          new_lower % chars ( icharacter ) = VSTRING_LOWER ( firstindex : firstindex )
       endif
    enddo
  end function vstring_tolower
  !
  ! vstring_totitle --
  !   Returns a value equal to string except that the first character in string is converted 
  !   to upper case. and the rest of the string is converted to lower case. If first is specified, it refers 
  !   to the first char index in the string to start modifying. If last is specified, it refers 
  !   to the char index in the string to stop at (inclusive).
  !
  function vstring_totitle ( this , first , last ) result ( new_title )
    type(t_vstring), intent(in) :: this
    integer , intent ( in ), optional :: first
    integer , intent ( in ), optional :: last
    type(t_vstring) :: new_title
    integer :: first_real
    integer :: last_real
    integer :: length
    !
    ! The new string is made of 4 parts :
    ! - before the first letter to update (left unchanged)
    ! - the letter to upper case
    ! - the sub-part to lower case
    ! - after the last letter to update (left unchanged)
    !
    type(t_vstring) :: subpart1
    type(t_vstring) :: subpart2
    type(t_vstring) :: subpart3
    type(t_vstring) :: subpart4
    type(t_vstring) :: subpartToUpper
    type(t_vstring) :: subpartToLower
    call vstring_check_string ( this , "vstring_totitle" )
    !
    ! Get options
    !
    if (present( first )) then
       call vstring_check_index ( this , first , "vstring_totitle" )
       first_real = first
    else
       first_real = 1
    endif
    length = vstring_length ( this )
    if (present( last )) then
       call vstring_check_index ( this , last , "vstring_totitle" )
       last_real = last
    else
       last_real = length
    endif
    !
    ! Compute subpart1
    !
    if ( first_real > 1 ) then
       subpart1 = vstring_range ( this , 1 , first_real - 1 )
    else
       call vstring_new ( subpart1 , "" )
    endif
    !
    ! Compute subpart2
    !
    if ( length > 0 ) then
       subpartToUpper = vstring_index ( this , first_real )
       subpart2 = vstring_toupper ( subpartToUpper )
       call vstring_free ( subpartToUpper )
    else
       call vstring_new ( subpart2 , "" )
    endif
    !
    ! Compute subpart3
    !
    if ( first_real + 1 <= last_real ) then
       subpartToLower = vstring_range ( this , first_real + 1, last_real )
       subpart3 = vstring_tolower ( subpartToLower )
       call vstring_free ( subpartToLower )
    else
       call vstring_new ( subpart3 , "" )
    endif
    !
    ! Compute subpart4
    !
    if ( last_real + 1 <= length ) then
       subpart4 = vstring_range ( this , last_real + 1 , length )
    else
       call vstring_new ( subpart4 , "" )
    endif
    !
    ! Concatenate the sub-parts
    !
    call vstring_new ( new_title , subpart1 )
    call vstring_append ( new_title , subpart2 )
    call vstring_append ( new_title , subpart3 )
    call vstring_append ( new_title , subpart4 )
    !
    ! Cleanup
    !
    call vstring_free ( subpart1 )
    call vstring_free ( subpart2 )
    call vstring_free ( subpart3 )
    call vstring_free ( subpart4 )
  end function vstring_totitle
  !
  ! vstring_reverse --
  !   Return a string that has all characters in reverse order
  ! Arguments:
  !   this     The current object
  ! Result:
  !   Reversed string
  !
  function vstring_reverse ( this ) result ( new_reverse )
    type(t_vstring), intent(in) :: this
    type(t_vstring) :: new_reverse
    integer :: icharacter
    integer :: length
    call vstring_check_string ( this , "vstring_reverse" )
    length = vstring_length ( this )
    call vstring_new ( new_reverse , this )
    do icharacter = 1 , length
       new_reverse % chars ( icharacter ) = this % chars ( length - icharacter + 1 )
    enddo
  end function vstring_reverse
  !
  ! vstring_random --
  !   Fill a string with a random sequence of letters
  ! Arguments:
  !   length : the length of the random string to compute.
  ! Result:
  !   String with random letters
  ! Note :
  !   This is a static method.
  !
  function vstring_random ( length ) result ( new_random )
    integer, intent(in) :: length
    type(t_vstring) :: new_random
    integer :: icharacter
    integer :: random_integer
    integer :: setsize
    real :: alea
    type(t_vstring) :: characterset
    !
    ! Initialize
    !
    setsize = len ( VSTRING_CHARACTERSET )
    call vstring_new ( characterset , VSTRING_CHARACTERSET )
    call vstring_new ( new_random , length )
    if ( .NOT.random_process_initialize ) then
       call random_seed()
       random_process_initialize = .true.
    endif
    !
    ! Compute each random character
    !
    do icharacter = 1 , length
       call random_number ( alea )
       random_integer = nint( alea * setsize + 1 - alea )
       new_random % chars ( icharacter ) = characterset % chars ( random_integer )
    enddo
    !
    ! Cleanup
    !
    call vstring_free ( characterset )
  end function vstring_random
  !
  ! vstring_achar --
  !   Static method
  !   Returns the character located at position I in the ASCII collating sequence.
  !   This is an interface to the standard fortran.
  !
  function vstring_achar ( i ) result ( new_achar )
    integer, intent (in) :: i
    character(len=1 ) :: thechar
    type (t_vstring) :: new_achar
    thechar = achar(i)
    call vstring_new ( new_achar , thechar )
  end function vstring_achar
  !
  ! vstring_char --
  !   Static method
  !   Returns the character represented by the integer I.
  !   This is an interface to the standard fortran.
  ! Example :
  !   If i is 64, then char is "@".
  !
  function vstring_char ( i ) result ( new_achar )
    integer, intent (in) :: i
    character(len=1 ) :: thechar
    type (t_vstring) :: new_achar
    thechar = char ( i )
    call vstring_new ( new_achar , thechar )
  end function vstring_char
  !
  ! vstring_iachar --
  !   Returns the code for the ASCII character in the character position of C.
  ! Arguments:
  !   this : the current string
  !   This is an interface to the standard fortran.
  ! Example :
  !   If this is "@", then iachar is 64.
  !
  function vstring_iachar ( this ) result ( new_iachar )
    type(t_vstring), intent(in) :: this
    integer :: new_iachar
    integer :: length
    character ( len = 200 ) :: message
    character :: thechar
    length = vstring_length ( this )
    if ( length/=1 ) then
       write ( message , * ) "The length of the current string is not 1 :", length , &
            " in vstring_iachar"
       call vstring_error ( this , message )
    endif
    call vstring_tocharstring ( this , 1 , thechar )
    new_iachar = iachar ( thechar )
  end function vstring_iachar
  !
  ! vstring_ichar --
  !   Returns the code for the character in the first character position of
  !   in the system’s native character set.
  ! Note
  !   This is an interface to the standard fortran.
  ! Arguments:
  !   this : the current string
  !
  function vstring_ichar ( this ) result ( new_ichar )
    type(t_vstring), intent(in) :: this
    integer :: new_ichar
    integer :: length
    character ( len = 200 ) :: message
    character :: thechar
    length = vstring_length ( this )
    if ( length/=1 ) then
       write ( message , * ) "The length of the current string is not 1 :", length , &
            " in vstring_ichar"
       call vstring_error ( this , message )
    endif
    call vstring_tocharstring ( this , 1 , thechar)
    new_ichar = ichar ( thechar )
  end function vstring_ichar
  !
  ! vstring_append --
  !   Append the given string at the end of the current string.
  !   If the given string (string_b) is of length greater than zero,
  !   that means that the length of the current string will be greater
  !   after the call to vstring_append.
  ! Note
  !   That method can be called as a convenient alternative to vstring_concat,
  !   when the concat is to be done "in place".
  !
  subroutine vstring_append ( this , string_b )
    type(t_vstring), intent(inout) :: this
    type(t_vstring), intent(in) :: string_b
    type(t_vstring) :: old_string
    call vstring_check_string ( this , "vstring_append" )
    call vstring_check_string ( string_b , "vstring_append" )
    call vstring_new ( old_string , this )
    call vstring_free ( this )
    this = vstring_concat ( old_string , string_b )
    call vstring_free ( old_string )
  end subroutine vstring_append
  !
  ! vstring_map --
  !   Replaces substrings in string based on the mapping defined by the couple (map_old , map_new). 
  !   map_old and map_new are arrays of vstrings and are of the same size so that
  !   if imap is an index no greater than the size of map_old, 
  !   map_old ( imap ) is the old string and map_new ( imap ) is the new string.
  !   Each instance of a key in the string will be replaced with its corresponding value. 
  !   Both old and new strings may be multiple characters. 
  !   If nocase is set to .true., then matching is done without regard to case differences. 
  !   Replacement is done in an ordered manner, so the old string appearing first 
  !   in the list will be checked first, and so on. The current string is only iterated over once, 
  !   so earlier replacements will have no affect for later matches.
  !   For example,
  !     vstring_map 1abcaababcabababc [abc,ab,a,1] [1,2,3,0]
  !   will return the string 01321221.
  !   Note that if an earlier key is a prefix of a later one, it will completely 
  !   mask the later one. So if the previous example is reordered like this,
  !     vstring_map 1abcaababcabababc [1,ab,a,abc] [0,2,3,1]
  !   it will return the string 02c322c222c.
  !
  function vstring_map ( this , map_old , map_new , nocase ) result ( stringmap )
    type(t_vstring), intent(inout) :: this
    type(t_vstring), dimension( : ), intent(in) :: map_old
    type(t_vstring), dimension( : ), intent(in) :: map_new
    logical, intent(in), optional :: nocase
    type(t_vstring) :: stringmap
    integer :: imap
    integer :: map_length
    integer :: map_length_new
    character ( len = 200) :: message
    logical :: mapping_done
    integer :: start
    integer :: first
    integer :: last
    type(t_vstring) :: replaced_string
    integer :: imap_to_apply
    logical :: is_map_left_to_apply
    integer :: map_old_first
    integer :: map_new_length
    logical :: nocase_real
    type(t_vstring) :: stringmap_lower
    type(t_vstring) :: mapold_lower
    !
    ! Check input data
    !
    call vstring_check_string ( this , "vstring_map" )
    map_length = size ( map_old )
    map_length_new = size ( map_new )
    if ( map_length /= map_length_new ) then
       write ( message , * ) "Map for old and new strings are not of the same length. ", &
            " Length map_old:", map_length , &
            " Length map_new:", map_length_new , &
            " in vstring_map"
       call vstring_error ( this , message , "vstring_map" )
    endif
    !
    ! Process options
    !
    if ( present ( nocase ) ) then
       nocase_real = nocase
    else
       nocase_real = .false.
    endif
    !
    ! Check map content
    !
    do imap = 1 , map_length
       call vstring_check_string ( map_old ( imap ) , "vstring_map" )
       call vstring_check_string ( map_new ( imap ) , "vstring_map" )
    enddo
    !
    ! Apply map
    !
    start = 1
    mapping_done = .false.
    call vstring_new ( stringmap , this )
    !
    ! Do a loop over the characters of the string.
    !
    do
       !
       ! Computes the matches for all maps and 
       ! store the map which is the most at the left of the string.
       !
       first = vstring_length ( stringmap ) + 1
       imap_to_apply = 0
       is_map_left_to_apply = .false.
       do imap = 1, map_length
          if (nocase_real) then
             stringmap_lower = vstring_tolower ( stringmap )
             mapold_lower = vstring_tolower ( map_old ( imap ) )
             map_old_first = vstring_first ( stringmap_lower , mapold_lower , first = start )
             call vstring_free ( stringmap_lower )
             call vstring_free ( mapold_lower )
          else
             map_old_first = vstring_first ( stringmap , map_old ( imap ) , first = start )
          endif
          if ( map_old_first > 0 .AND. map_old_first < first ) then
             is_map_left_to_apply = .true.
             imap_to_apply = imap
             first = map_old_first
          endif
       enddo
       !
       ! Apply the match which is the left most.
       !
       if ( is_map_left_to_apply ) then
          last = first + vstring_length ( map_old ( imap_to_apply ) ) - 1
          replaced_string = vstring_replace ( stringmap , first , last ,  map_new ( imap_to_apply ) )
          call vstring_free ( stringmap )
          call vstring_new ( stringmap , replaced_string )
          call vstring_free ( replaced_string )
          map_new_length = vstring_length ( map_new ( imap_to_apply ) )
          start = start + map_new_length
          if ( start > vstring_length ( stringmap ) ) then
             !
             ! There are no characters left to map.
             !
             mapping_done = .true.
          endif
       else
          mapping_done = .true.
       endif
       if ( mapping_done ) then
          exit
       endif
    enddo
    !
    ! Cleanup
    !

  end function vstring_map
  !
  ! vstring_replace --
  !   Removes a range of consecutive characters from string, starting with the character whose 
  !   index is first and ending with the character whose index is last. An index of 1 refers to 
  !   the first character of the string. 
  !   If newstring is specified, then it is placed in the removed character range.
  !
  function vstring_replace ( this , first , last , newstring ) result ( stringreplace )
    type(t_vstring), intent(inout) :: this
    integer, intent(in) :: first
    integer, intent(in) :: last
    type(t_vstring), intent(in), optional :: newstring
    type(t_vstring) :: stringreplace
    type(t_vstring) :: part1
    type(t_vstring) :: part2
    type(t_vstring) :: newstring_real
    integer :: length
    call vstring_check_string ( this , "vstring_replace" )
    call vstring_check_index ( this , first , "vstring_replace" )
    call vstring_check_index ( this , last , "vstring_replace" )
    !
    ! Get optional arguments
    !
    if ( present ( newstring ) ) then
       call vstring_new ( newstring_real , newstring )
    else
       call vstring_new ( newstring_real , "" )
    endif
    !
    ! Compute part #1
    !
    if ( first > 1 ) then
       part1 = vstring_range ( this , 1 , first - 1 )
    else
       call vstring_new ( part1 , "" )
    endif
    !
    ! Compute part #2
    !
    length = vstring_length ( this )
    if ( last < length ) then
       part2 = vstring_range ( this , last + 1 , length )
    else
       call vstring_new ( part2 , "" )
    endif
    !
    ! Concatenate the result
    !
    call vstring_new ( stringreplace , part1 )
    call vstring_append ( stringreplace , newstring_real )
    call vstring_append ( stringreplace , part2 )
    !
    ! Cleanup
    !
    call vstring_free ( part1 )
    call vstring_free ( part2 )
    call vstring_free ( newstring_real )
  end function vstring_replace
  !
  ! vstring_charindex --
  !   Returns the position of the start of the first occurrence of string SUBSTRING
  !   as a substring in the current string, counting from one. If SUBSTRING is not present
  !   in the current string, zero is returned. If the BACK argument is present and true, the
  !   return value is the start of the last occurrence rather than the first.
  ! Note :
  !   This is a simple interface to the standard fortran intrinsic "index".
  !
  function vstring_charindex ( this , substring , back ) result ( charindex )
    type(t_vstring), intent(in)   :: this
    type(t_vstring), intent(in)   :: substring
    logical, intent(in), optional :: back
    integer :: charindex
    character ( len = vstring_length ( this ) ) :: this_charstring
    character ( len = vstring_length ( substring ) ) :: substring_charstring
    logical :: back_real
    if ( present ( back ) ) then
       back_real = back
    else
       back_real = .false.
    endif
    call vstring_tocharstring ( this , this_charstring )
    call vstring_tocharstring ( substring , substring_charstring )
    charindex = index ( this_charstring , substring_charstring , back_real )
  end function vstring_charindex
  !
  ! vstring_scan --
  !   Returns the position of a character of the current string that is in set, or zero
  !   if there is no such character. If the logical back is absent or present with value
  !   false, the position of the leftmost such character is returned. If back is present
  !   with value true, the position of the rightmost such character is returned.
  ! Note :
  !   This is a simple interface to the standard fortran intrinsic "scan".
  !
  function vstring_scan ( this , substring , back ) result ( charindex )
    type(t_vstring), intent(in)   :: this
    type(t_vstring), intent(in)   :: substring
    logical, intent(in), optional :: back
    integer :: charindex
    character ( len = vstring_length ( this ) ) :: this_charstring
    character ( len = vstring_length ( substring ) ) :: substring_charstring
    logical :: back_real
    if ( present ( back ) ) then
       back_real = back
    else
       back_real = .false.
    endif
    call vstring_tocharstring ( this , this_charstring )
    call vstring_tocharstring ( substring , substring_charstring )
    charindex = scan ( this_charstring , substring_charstring , back_real )
  end function vstring_scan
  !
  ! vstring_verify --
  !   Returns the default integer value 0 if each character in the current 
  !   string appears in set, or the position of a character of the current string
  !   that is not in set. If the logical back is absent or present with value false,
  !   the position of the left-most such character is returned. If back is 
  !   present with value true, the position of the rightmost such character is returned.
  ! Note :
  !   This is a simple interface to the standard fortran intrinsic "verify".
  !
  function vstring_verify ( this , substring , back ) result ( charindex )
    type(t_vstring), intent(in)   :: this
    type(t_vstring), intent(in)   :: substring
    logical, intent(in), optional :: back
    integer :: charindex
    character ( len = vstring_length ( this ) ) :: this_charstring
    character ( len = vstring_length ( substring ) ) :: substring_charstring
    logical :: back_real
    if ( present ( back ) ) then
       back_real = back
    else
       back_real = .false.
    endif
    call vstring_tocharstring ( this , this_charstring )
    call vstring_tocharstring ( substring , substring_charstring )
    charindex = verify ( this_charstring , substring_charstring , back_real )
  end function vstring_verify
  !
  ! vstring_adjustl --
  !   Adjusts left to return a string of the same length by removing 
  !   all leading blanks and inserting the same number of trailing blanks.
  ! Note :
  !   This is a simple interface to the standard fortran intrinsic "adjustl".
  !
  function vstring_adjustl ( this ) result ( newstring )
    type(t_vstring), intent(in)   :: this
    type(t_vstring) :: newstring
    character ( len = vstring_length ( this ) ) :: this_charstring
    character ( len = vstring_length ( this ) ) :: adjusted
    call vstring_tocharstring ( this , this_charstring )
    adjusted = adjustl ( this_charstring (:) )
    call vstring_new ( newstring , adjusted )
  end function vstring_adjustl
  !
  ! vstring_adjustr --
  !   Adjusts right to return a string of the same length by removing 
  !   all trailing blanks and inserting the same number of leading blanks.
  ! Note :
  !   This is a simple interface to the standard fortran intrinsic "adjustr".
  !
  function vstring_adjustr ( this ) result ( newstring )
    type(t_vstring), intent(in)   :: this
    type(t_vstring) :: newstring
    character ( len = vstring_length ( this ) ) :: this_charstring
    character ( len = vstring_length ( this ) ) :: adjusted
    call vstring_tocharstring ( this , this_charstring )
    adjusted = adjustr ( this_charstring )
    call vstring_new ( newstring , adjusted )
  end function vstring_adjustr
  !
  ! vstring_match --
  !   See if pattern matches string; return 1 if it does, 0 if it doesn't. 
  !   If -nocase is specified, then the pattern attempts to match against the 
  !   string in a case insensitive manner. 
  !   For the two strings to match, their contents must be identical except 
  !   that the following special sequences may appear in pattern:
  !     *
  !       Matches any sequence of characters in string, including a null string.
  !     ?
  !       Matches any single character in string.
  !     [chars]
  !       Matches any character in the set given by chars. 
  !       If a sequence of the form x-y appears in chars, then any character 
  !       between x and y, inclusive, will match. 
  !       When used with -nocase, the characters of the range are converted to lower case first. 
  !       Whereas {[A-z]} matches '_' when matching case-sensitively ('_' falls between the 'Z' 
  !       and 'a'), with -nocase this is considered like {[A-Za-z]} (and probably what was 
  !       meant in the first place).
  !     \x
  !       Matches the single character x. 
  !       This provides a way of avoiding the special interpretation of the characters *?[]\ in pattern.
  !
  recursive function vstring_match ( this , pattern , nocase ) result ( match )
    type(t_vstring), intent(in) :: this
    type(t_vstring), intent(in) :: pattern
    logical , intent(in) , optional :: nocase
    logical :: match
    logical :: equals
    type(t_vstring) :: backslash
    type(t_vstring) :: question
    type(t_vstring) :: this_char1
    type(t_vstring) :: pattern_char1
    type(t_vstring) :: charstar
    type(t_vstring) :: leftbracket
    type(t_vstring) :: rightbracket
    integer :: this_length
    integer :: pattern_length
    type(t_vstring) :: this_substring
    type(t_vstring) :: pattern_substring
    type(t_vstring) :: pattern_char2
    type(t_vstring) :: pattern_substring2
    integer :: first
    integer :: firstrightbracket
    type(t_vstring) :: chars
    type(t_vstring) :: expanded
    logical :: nocase_real
    type(t_vstring) :: this_char1lower
    type(t_vstring) :: chars_lower
    !
    ! Process options
    !
    if ( present ( nocase ) ) then
       nocase_real = nocase
    else
       nocase_real = .false.
    endif
    !
    ! Initialize
    !
    call vstring_check_string ( this , "Check string in vstring_match." )
    call vstring_check_string ( pattern , "Check pattern in vstring_match." )
    call vstring_new ( charstar , "*" )
    call vstring_new ( backslash , "\" )
    call vstring_new ( question , "?" )
    call vstring_new ( leftbracket , "[" )
    call vstring_new ( rightbracket , "]" )
    this_length = vstring_length ( this )
    pattern_length = vstring_length ( pattern )
    !
    ! Get various parts of the current string and the pattern :
    ! - first char,
    ! - everything from the 2 to the end.
    !
    if ( this_length > 0 ) then
       this_char1 = vstring_index ( this , 1 )
    else
       call vstring_new ( this_char1 , "" )
    endif
    if ( pattern_length > 0 ) then
       pattern_char1 = vstring_index ( pattern , 1 )
    else
       call vstring_new ( pattern_char1, "" )
    endif
    if ( this_length > 1 ) then
       this_substring = vstring_range ( this , 2 , this_length )
    else
       call vstring_new ( this_substring , "" )
    endif
    if ( pattern_length > 1 ) then
       pattern_substring = vstring_range ( pattern , 2 , pattern_length )
    else
       call vstring_new ( pattern_substring , "" )
    endif
    !
    ! This is a chain of tests :
    ! the first test which returns .true. breaks the chain.
    !
    match = .false.
    do
       !
       ! Process the strict equality between string and pattern
       !
       match = vstring_equals ( this , pattern , nocase = nocase_real )
       if ( match ) then
          exit
       endif
       !
       ! Process the case where the first character of the pattern is a "\".
       !
       equals = vstring_equals ( backslash , pattern_char1 )
       if ( equals ) then
          !
          ! Compare the second character of the pattern against the first character of the string.
          !
          if ( pattern_length > 1 ) then
             pattern_char2 = vstring_index ( pattern , 2 )
             equals = vstring_equals ( this_char1 , pattern_char2 , nocase = nocase_real )
             call vstring_free ( pattern_char2 )
             if ( equals ) then
                if ( pattern_length > 2 ) then
                   pattern_substring2 = vstring_range ( pattern , 3 , pattern_length )
                else
                   call vstring_new ( pattern_substring2 , "" )
                endif
                match = vstring_match ( this_substring , pattern_substring2 , nocase = nocase_real )
                call vstring_free ( pattern_substring2 )
                if ( match ) then
                   exit
                endif
             endif
          endif
       endif
       !
       ! Process the case where the first character of the pattern is a "*"
       !
       equals = vstring_equals ( charstar , pattern_char1 )
       if ( equals ) then
          !
          ! Solution #1 : Compare the string against the end of the pattern
          !
          match = vstring_match ( this , pattern_substring , nocase = nocase_real )
          if ( match ) then
             exit
          endif
          !
          ! Solution #2 : Compare the end of the string against the pattern
          !
          match = vstring_match ( this_substring , pattern , nocase = nocase_real )
          if ( match ) then
             exit
          endif
       endif
       !
       ! Process the case where the first character of the pattern is a "?"
       ! If the current string is of length 0, there is no match.
       !
       equals = vstring_equals ( question , pattern_char1 )
       if ( equals .AND. this_length > 0 ) then
          !
          ! Compare the end of the string against the end of the pattern.
          !
          match = vstring_match ( this_substring , pattern_substring , nocase = nocase_real )
          if ( match ) then
             exit
          endif
       endif
       !
       ! Process the case where the first character of the pattern is a "[".
       ! It is followed by a list of characters that are acceptable, or by a range
       ! (two characters separated by "-").
       ! It is ended by a "]".
       !
       equals = vstring_equals ( leftbracket , pattern_char1 )
       if ( equals ) then
          !
          ! Search for the corresponding right bracket "]"
          !
          firstrightbracket = vstring_first ( pattern , rightbracket )
          if ( firstrightbracket > 2 ) then
             !
             ! Get the characters in the set.
             !
             chars = vstring_range ( pattern , 2 , firstrightbracket - 1 )
             if ( nocase_real ) then
                chars_lower = vstring_tolower ( chars )
                call vstring_free ( chars )
                call vstring_new ( chars , chars_lower )
                call vstring_free ( chars_lower )
             endif
             !
             ! Expand the character set, by taking into account for character ranges defined with "-".
             !
             expanded = vstring_expandcharset ( chars )
             call vstring_free ( chars )
             !
             ! Search the first char of the current string in the expanded set of characters
             !
             if ( nocase_real ) then
                this_char1lower = vstring_tolower ( this_char1 )
                first = vstring_first ( expanded , this_char1lower )
                call vstring_free ( this_char1lower )
             else
                first = vstring_first ( expanded , this_char1 )
             endif
             call vstring_free ( expanded )
             if ( first > 0 ) then
                !
                ! Compare the end of the string to the end of the pattern
                !
                if ( pattern_length > firstrightbracket ) then
                   pattern_substring2 = vstring_range ( pattern , firstrightbracket + 1 , pattern_length )
                else
                   call vstring_new ( pattern_substring2 , "" )
                endif
                match = vstring_match ( this_substring , pattern_substring2 , nocase = nocase_real )
                call vstring_free ( pattern_substring2 )
                if ( match ) then
                   exit
                endif
             endif
          endif
       endif
       !
       ! The first letter is not a special character.
       ! There is a match if the first letters are the same
       ! and the end of both string and pattern match.
       !
       equals = vstring_equals ( this_char1 , pattern_char1 , nocase = nocase_real )
       if ( equals ) then
          !
          ! Compare the end of the string against the end of the pattern
          !
          match = vstring_match ( this_substring , pattern_substring , nocase = nocase_real )
          if ( match ) then
             exit
          endif
       endif
       !
       ! No test match
       !
       exit
    enddo
    call vstring_free ( charstar )
    call vstring_free ( backslash )
    call vstring_free ( question )
    call vstring_free ( this_char1 )
    call vstring_free ( pattern_char1 )
    call vstring_free ( this_substring )
    call vstring_free ( pattern_substring )
    call vstring_free ( leftbracket )
    call vstring_free ( rightbracket )
  end function vstring_match
  !
  ! vstring_expandcharset --
  !   Consider that the current string is a character set and returns 
  !   the expanded form of that set.
  !   The expanded form of the character set "abc" is "abc".
  !   If a sequence of the form x-y appears in the current string, then the expanded form
  !   contains all characters between x and y, inclusive.
  !   For example, if the current string is "a-z", the returned expanded set is made 
  !   of all the lower case letters.
  !   For example, if the current string is "a-zA-Z", the returned expanded set is made 
  !   of all the lower case letters and upper case letters.
  !   For example, if the current string is "a-zA-Z_", the returned expanded set is made 
  !   of all the lower case letters and upper case letters and underscore.
  !
  recursive function vstring_expandcharset ( this ) result ( expanded )
    implicit none
    type(t_vstring), intent(in) :: this
    type(t_vstring) :: expanded
    type(t_vstring) :: minus
    integer :: firstminus
    type(t_vstring) :: startchar
    type(t_vstring) :: endchar
    integer :: this_length
    integer :: startascii
    integer :: endascii
    integer :: iascii
    type(t_vstring) :: asciichar
    type(t_vstring) :: this_substring
    type(t_vstring) :: expanded_end
    !
    ! Initialize
    !
    call vstring_check_string ( this , "vstring_expandcharset" )
    call vstring_new ( minus , "-" )
    this_length = vstring_length ( this )
    !
    ! Compute expanded character set
    !
    firstminus = vstring_first ( this , minus )
    if ( firstminus == 2 .AND. this_length >= 3) then
       !
       ! Add the current range to the expanded set.
       !
       startchar = vstring_index ( this , 1 )
       endchar = vstring_index ( this , 3 )
       startascii = vstring_iachar ( startchar )
       endascii = vstring_iachar ( endchar )
       !
       ! Add the the character set all character which ascii index is between start and end.
       !
       call vstring_new ( expanded , "" )
       do iascii = startascii , endascii
          asciichar = vstring_char ( iascii )
          call vstring_append ( expanded , asciichar )
          call vstring_free ( asciichar )
       enddo
       call vstring_free ( startchar )
       call vstring_free ( endchar )
       !
       ! Expand the end of the character set.
       !
       if ( this_length > 3 ) then
          this_substring = vstring_range ( this , 4 , this_length )
          expanded_end = vstring_expandcharset ( this_substring )
          call vstring_append ( expanded , expanded_end )
          call vstring_free ( expanded_end )
          call vstring_free ( this_substring )
       endif
    else
       call vstring_new ( expanded , this )
    endif
    !
    ! Clean-up
    !
    call vstring_free ( minus )
  end function vstring_expandcharset
  !
  ! vstring_split --
  !   Computes an array whose elements are the components in the current string.
  !   Returns an array created by splitting string at each character that is in 
  !   the splitChars argument. Each element of the result array will consist of 
  !   the characters from string that lie between instances of the characters in 
  !   splitChars. The numberOfComponents is zero if string contains adjacent 
  !   characters in splitChars. If splitChars is an empty string then each character of string 
  !   becomes a separate element of the result list. SplitChars defaults to the 
  !   standard white-space characters.
  ! Arguments:
  !   this   The current string to process
  !   numberOfComponents : the number of items in the list of components
  !   splitted   The array of splitted names.
  !
  subroutine vstring_split ( this , numberOfComponents , listOfComponents  , splitChars )
    implicit none
    type(t_vstring), intent(in) :: this
    type(t_vstring), intent(in), optional :: splitChars
    integer , intent ( out ) :: numberOfComponents
    type(t_vstring), dimension (:), pointer :: listOfComponents
    
    integer :: firstsplitchar
    integer , parameter :: numberOfSteps = 2
    integer :: istep
    type(t_vstring) :: splitChars_real
    type(t_vstring) :: component
    integer :: splitChars_real_length
    integer :: this_length
    integer :: icharacter
    type(t_vstring) :: current_char
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
       numberOfComponents = this_length
       allocate ( listOfComponents ( 1 : numberOfComponents ) )
       do icharacter = 1 , this_length
          component = vstring_index ( this , icharacter )
          call vstring_new ( listOfComponents ( icharacter ) , component )
          call vstring_free ( component )
       enddo
    else
       !
       ! Algorithm is in two steps :
       ! Step #1 : count the number of components
       ! Step #2 : store the components
       !
       do istep = 1 , numberOfSteps
          numberOfComponents = 0
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
                numberOfComponents = numberOfComponents + 1
                if ( istep == 2 ) then
                   call vstring_new ( listOfComponents ( numberOfComponents ) , component )
                endif
                call vstring_free ( component )
                call vstring_new ( component , "" )
             else
                call vstring_append ( component , current_char )
             endif
             call vstring_free ( current_char )
          enddo
          if ( istep == 1 ) then
             !
             ! Allocate the array
             !
             ! This is because of the last component.
             component_length = vstring_length ( component )
             if ( component_length > 0 ) then
                numberOfComponents = numberOfComponents + 1
             endif
             allocate ( listOfComponents ( 1 : numberOfComponents ) )
          endif
          if ( istep == 2 ) then
             ! Store the last component
             component_length = vstring_length ( component )
             if ( component_length > 0 ) then
                numberOfComponents = numberOfComponents + 1
                call vstring_new ( listOfComponents ( numberOfComponents ) , component )
             endif
          endif
          call vstring_free ( component )
       enddo
    endif
    !
    ! Clean-up
    !
    call vstring_free ( splitChars_real )
  end subroutine vstring_split
  !
  ! vstring_check_index --
  !   Check that the given index is correct and generates an error if not.
  !
  subroutine vstring_check_index ( this , charIndex , origin )
    implicit none
    type(t_vstring), intent(in) :: this
    integer, intent(in)         :: charIndex
    character ( len = * ), intent(in), optional :: origin
    character ( len = 200 ) :: message
    integer :: length
    call vstring_check_string ( this , "vstring_check_index" )
    length = vstring_length ( this )
    if ( charIndex < 1 ) then
       write ( message , * ) "Character index ", charIndex , " is less that 1 in vstring_check_index"
       if ( present ( origin ) ) then
          call vstring_error ( this , message , origin )
       else
          call vstring_error ( this , message )
       endif
    elseif ( charIndex > length ) then
       write ( message , * ) "Character index ", charIndex , " is greater that the length ", length , &
            " in vstring_check_index"
       if ( present ( origin ) ) then
          call vstring_error ( this , message , origin )
       else
          call vstring_error ( this , message )
       endif
    endif
  end subroutine vstring_check_index
  !
  ! vstring_check_string --
  !   Check that the given string is correct and generates an error if not.
  !
  subroutine vstring_check_string ( this , origin )
    type(t_vstring), intent(in) :: this
    character ( len = 200) :: message
    character ( len = * ), intent(in), optional :: origin
    logical :: stringallocated
    stringallocated = vstring_allocated ( this )
    if ( .NOT.stringallocated ) then
       write ( message , * ) "String is not allocated, size :", size ( this % chars ) , &
            " in vstring_check_string"
       if ( present ( origin ) ) then
          call vstring_error ( this , message , origin )
       else
          call vstring_error ( this , message )
       endif
    endif
  end subroutine vstring_check_string
  !
  ! vstring_error --
  !   Generates an error with the given error message
  !
  subroutine vstring_error ( this , message , origin )
    implicit none
    type(t_vstring), intent(in) :: this
    character ( len = * ), intent (in) :: message
    character ( len = * ), intent(in), optional :: origin
    integer :: length
    integer, parameter :: length_max = 40
    logical :: isallocated
    write ( 6 , * ) "Internal error in m_vstring"
    if ( present ( origin ) ) then
       write ( 6 , * ) "Origin : ", origin
    endif
    length =  vstring_length ( this )
    isallocated = vstring_allocated ( this )
    if ( isallocated ) then
       write ( 6 , * ) "Length:" , length
       if ( length < length_max ) then
          write ( 6 , * ) "Content:", this % chars
       else
          write ( 6 , * ) "Content:", this % chars(1:length_max), "..."
       endif
    endif
#ifdef _VSTRING_ALLOCATABLE
    write ( 6 , * ) "Version : allocatable"
#endif
#ifdef _VSTRING_POINTERS
    write ( 6 , * ) "Version : pointer"
#endif
    write ( 6 , * ) "Message :", trim( message )
    STOP
  end subroutine vstring_error
end module m_vstring

