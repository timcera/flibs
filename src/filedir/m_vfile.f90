!
! m_vfile.f90 --
!
!   Manipulate file and directory names.
!   This component is based on a dynamic strings so that the 
!   file or directory name may be defined with no limit in the 
!   number of characters.
!
! Overview
!
!   This component allows to manage the file system, by providing 
!   services to create, move and destroy files and directories, and 
!   to get informations about files and directories.
!   The services provided are based either on standard fortran,
!   or on fortran extensions.
!
! Portability
!
!   One of the main interest of this component is to separate
!   the client-side application from platform-specific file 
!   management or from compiler-specific fortran extensions. 
!
!   This separation is possible because m_vfile
!   deals for the platform directly, by using the m_platform
!   module. This allows to design a client source code which 
!   portable on several operating systems (for example windows,
!   linux) without any change. For example, the several file 
!   separators used on the various operating systems are taken
!   into account internally : "/" on linux systems, "\" on 
!   windows systems and ":" on Mac OS.
!
!   The portability is also ensured with respect to the 
!   fortran compiler used to create the executable.
!   All fortran compilers provide commands to rename the files
!   or get the working directory. But not all fortran compilers
!   define these commands the same way : some provide subroutines,
!   some provide functions, etc... The current component can 
!   be configured at compile-time with pre-processing commands.
!   This allows to configure the component with compiler 
!   specific settings, to make so that the component know
!   what features your particular compiler knows about.
!
!     <your application> 
!            |
!        m_vfile -> (operating system , fortran compiler)
!            |
!        m_platform -> (operating system , fortran compiler)
!
! How to use it
!
!   Before using the services provided by m_vfile, the client
!   code must call vfile_startup which initializes platform-specific 
!   commands. After using these services, the client code should call
!   vfile_shutdown.
!
!   The commands vfile_delete, vfile_copy, vfile_rename
!   allow to delete, copy and rename files or directories.
!   To inquire about a file or directory, one can use 
!   vfile_exists or vfile_isdirectory.
!  
!   In the following example, one creates a new file with vfile_touch,
!   rename that file and finally delete it.
!
!     call vfile_startup ()
!     call vstring_new ( file , "foo.txt" )
!     call vfile_touch ( file )
!     call vfile_rename ( file , "toto.txt" )
!     call vfile_delete ( file )
!     call vstring_free ( file )
!     call vfile_shutdown ()
!
!   The component makes no differences between file names 
!   and directory names, except for methods which are specific 
!   for file or for directory.
!
!   The vfile_separator returns the platform-specific character 
!   used on the current operating system.
!
!   The commands vfile_nativename , vfile_normalize , vfile_pathtype
!   provide ways to manage file names and paths.
!   The vfile_nativename function returns the platform-specific name of the file. 
!   The vfile_pathtype command returns one of VFILE_PATHTYPE_ABSOLUTE, 
!   VFILE_PATHTYPE_RELATIVE, VFILE_PATHTYPE_VOLUMERELATIVE which correspond to 
!   the current file. The VFILE_PATHTYPE_VOLUMERELATIVE only exist on 
!   windows. The vfile_normalize command returns a unique normalized 
!   path representation for the file-system object (file, directory, link, 
!   etc), whose string value can be used as a unique identifier for it.
!
!   The vfile_split and vfile_join services allows to separate
!   or concatenate the components of a file. This can be useful
!   when dealing with relative file or directories.
!   The vfile_split command splits a file into pieces each time 
!   the platform-specific separator is found.
!   The vfile_join command concatenate a list of strings with 
!   the platform-specific separator and returns the concatenated
!   file name.
!
!   One particularly useful command when dealing with files is 
!   vfile_findByPattern. The command takes a string as an input
!   file pattern. It then computes the list of all files which
!   match that pattern.
!
! Error management
!
!   The file management may raise errors, for example when the 
!   user want to rename a file which does not exist.
!   Many of the provided commands have an optional integer output 
!   argument "status" which is zero when no error occurred 
!   and non-zero in case of error.
!   If the status argument is not provided and an error is generated,
!   then the program stops and a message is displayed on standard 
!   output.
!   These are the public error flags that the current component may generate :
!     VFILE_ERROR_OK = 0
!     VFILE_ERROR_UNABLE_TO_OPEN_SOURCE
!     VFILE_ERROR_UNABLE_TO_OPEN_TARGET
!     VFILE_ERROR_UNABLE_TO_WRITE_TARGET
!     VFILE_ERROR_SOURCE_FILE_DOES_NOT_EXIST
!
!
!   Intel Fortran portability
!   Several methods of this component are based on Fortran extensions,
!   which requires compiler-specific settings.
!   For Intel Fortran compiler, the current implementation was based on
!   IFPORT.F90 file in the Intel release for details on the interfaces provided.
!   If the client code use these routines, it must define the pre-processing 
!   macro _VFILE_INTEL_FORTRAN_PORTABILITY_ROUTINES
!
!   File rename fortran extension.
!   Depending on the compiler, the "RENAME" fortran extension is 
!   provided as a subroutine or a function.
!   For example, this is a short list of compilers and their particular 
!   RENAME provided :
!   - function : Intel Fortran, g95
!   - subroutine : gfortran
!   Choose your RENAME version between one of these :
!   _VFILE_RENAME_FUNCTION , _VFILE_RENAME_SUBROUTINE
!
!   Dynamic or static buffer
!   The internal algorithms provided by m_vstrings are based on 
!   basic fortran character strings. In several situations, the 
!   dynamic vstring has to be converted into a basic fortran character
!   buffer string, which size has to be given explicitly in the source 
!   code, with "character ( len = <something>)" statement. 
!   Two solutions are provided, and the user can define the pre-processing macro 
!   _VFILE_STATIC_BUFFER to configure that :
!   - the first solution is to set the size of the buffer statically,
!     to a constant integer value VSTRING_BUFFER_SIZE.
!   - the second solution is to compute the size 
!     of the buffer dynamically, with the fortran 90 len = vstring_length(this)
!     statement,
!   If the _VFILE_STATIC_BUFFER is defined, then character strings of 
!   constant size are used as buffers.
!   If the _VFILE_STATIC_BUFFER is not defined (which is the default), 
!   then character strings of dynamic size are used as buffers.
!   The second solution is more efficient, because the strings are not 
!   oversized or undersized, depending on the real number of characters
!   in the dynamic string. But the feature may not be provided 
!   by the compiler at hand. For example, problems with the dynamic 
!   length character string have been experienced with Intel Fortran 8.
!
!   Preprocessing
!   The following preprocessing macro must be considered :
!   _VFILE_STATIC_BUFFER : see  the section "Dynamic or static buffer"
!   _VFILE_RENAME_FUNCTION or _VFILE_RENAME_SUBROUTINE : see the section "File rename fortran extension"
!   _VFILE_GETCWD_FUNCTION or _VFILE_GETCWD_SUBROUTINE
!
!   This is an abstract of all macros for several compilers.
!
!   Compiler : Intel Fortran V8.0
!   _VFILE_INTEL_FORTRAN_PORTABILITY_ROUTINES
!   _VFILE_RENAME_FUNCTION
!   _VFILE_STATIC_BUFFER
!   _VFILE_GETCWD_FUNCTION
!
!   Compiler : g95
!   _VFILE_RENAME_FUNCTION
!   _VFILE_GETCWD_FUNCTION
!
!   Compiler : gfortran
!   _VFILE_RENAME_SUBROUTINE
!   _VFILE_GETCWD_SUBROUTINE
!
! Copyright (c) 2008 Arjen Markus
! Copyright (c) 2008 Michael Baudin
!
!   $Id$
!
module m_vfile
#ifdef _VFILE_INTEL_FORTRAN_PORTABILITY_ROUTINES
  use ifport
#endif
  use m_platform, only : &
       platform_get_platform, &
       PLATFORM_PLATFORM_WINDOWS, &
       PLATFORM_PLATFORM_UNIX, &
       PLATFORM_PLATFORM_MAC, &
       PLATFORM_PLATFORM_NB
  use m_vstring, only : &
       t_vstring , &
       vstring_new , &
       vstring_free , &
       vstring_first , &
       vstring_range , &
       vstring_last , &
       vstring_length , &
       vstring_trim , &
       vstring_append , &
       vstring_cast , &
       vstring_exists , &
       vstring_equals , &
       vstring_index , &
       vstring_achar , &
       vstring_match
  use m_vstringformat , only : &
       vstring_format
  use m_vstringlist, only : &
       t_vstringlist , &
       vstrlist_new , &
       vstrlist_free , &
       vstrlist_append , &
       vstrlist_split , &
       vstrlist_index , &
       vstrlist_length , &
       vstrlist_search
  use m_vstrplatform, only : &
       vstrplatform_system, &
       vstrplatform_cd, &
       vstrplatform_stat , &
       vstrplatform_osstring , &
       vstrplatform_platformstring , &
       vstrplatform_get_environment_variable
  use m_fileunit, only : &
       fileunit_getfreeunit
  implicit none
  private
  public :: vfile_add_extension
  public :: vfile_atime
  public :: vfile_copy
  public :: vfile_delete
  public :: vfile_dirname
  public :: vfile_exists
  public :: vfile_extension
  public :: vfile_findByPattern
  public :: vfile_first_separator_index
  public :: vfile_isdirectory
  public :: vfile_join
  public :: vfile_last_separator_index
  public :: vfile_mkdir
  public :: vfile_mtime
  public :: vfile_nativename
  public :: vfile_normalize
  public :: vfile_pathtype
  public :: vfile_pwd
  public :: vfile_rename
  public :: vfile_rootname
  public :: vfile_separator
  public :: vfile_set_stoponerror
  public :: vfile_split
  public :: vfile_startup
  public :: vfile_tail
  public :: vfile_tempdir
  public :: vfile_tempfile
  public :: vfile_touch
  public :: vfile_shutdown
  public :: vfile_volumes
  !
  ! vfile_join --
  !   Generic join method
  !
  interface vfile_join
     module procedure vfile_join_vstring
     module procedure vfile_join_charstring
  end interface vfile_join
  !
  ! vfile_findByPattern --
  !   Generic file finder.
  !
  interface vfile_findByPattern
     module procedure vfile_findByPattern_vstring
     module procedure vfile_findByPattern_charstring
  end interface vfile_findByPattern
  !
  ! vfile_add_extension --
  !   Generic method to add an extension.
  !
  interface vfile_add_extension
     module procedure vfile_add_extension_vstring
     module procedure vfile_add_extension_charstring
  end interface vfile_add_extension
  !
  ! vfile_rootname --
  !   Generic method to compute the rootname.
  !
  interface vfile_rootname
     module procedure vfile_rootname_vstring
     module procedure vfile_rootname_charstring
  end interface vfile_rootname
  !
  ! vfile_extension --
  !   Generic method to compute the extension.
  !
  interface vfile_extension
     module procedure vfile_extension_vstring
     module procedure vfile_extension_charstring
  end interface vfile_extension
  !
  ! vfile_tail --
  !   Generic method to compute the tail.
  !
  interface vfile_tail
     module procedure vfile_tail_vstring
     module procedure vfile_tail_charstring
  end interface vfile_tail
  !
  ! vfile_dirname --
  !   Generic method to compute the directory name
  !
  interface vfile_dirname
     module procedure vfile_dirname_vstring
     module procedure vfile_dirname_charstring
  end interface vfile_dirname
  !
  ! vfile_last_separator_index --
  !   Generic method to return the index of the last separator in the filename
  !
  interface vfile_last_separator_index
     module procedure vfile_last_separator_index_vstring
     module procedure vfile_last_separator_index_charstring
  end interface vfile_last_separator_index
  !
  ! vfile_first_separator_index --
  !   Generic method to return the index of the first separator in the filename
  !
  interface vfile_first_separator_index
     module procedure vfile_first_separator_index_vstring
     module procedure vfile_first_separator_index_charstring
  end interface vfile_first_separator_index
  !
  ! vfile_exists --
  !   Generic method which returns .true. if the file exists.
  !
  interface vfile_exists
     module procedure vfile_exists_vstring
     module procedure vfile_exists_charstring
  end interface vfile_exists
  !
  ! vfile_rename --
  !   Generic method to rename a file.
  !
  interface vfile_rename
     module procedure vfile_rename_vstring
     module procedure vfile_rename_charstring
  end interface vfile_rename
  !
  ! vfile_copy --
  !   Generic method to copy a file.
  !
  interface vfile_copy
     module procedure vfile_copy_vstring
     module procedure vfile_copy_charstring
  end interface vfile_copy
  !
  ! vfile_delete --
  !   Generic method to delete a file.
  !
  interface vfile_delete
     module procedure vfile_delete_vstring
     module procedure vfile_delete_charstring
  end interface vfile_delete
  !
  ! vfile_isdirectory --
  !   Generic method which returns .true. if the string is a directory.
  !
  interface vfile_isdirectory
     module procedure vfile_isdirectory_vstring
     module procedure vfile_isdirectory_charstring
  end interface vfile_isdirectory
  !
  ! vfile_atime --
  !   Generic method  which returns the access time of the file.
  !
  interface vfile_atime
     module procedure vfile_atime_vstring
     module procedure vfile_atime_charstring
  end interface vfile_atime
  !
  ! vfile_mtime --
  !   Generic method  which returns the modification time of the file.
  !
  interface vfile_mtime
     module procedure vfile_mtime_vstring
     module procedure vfile_mtime_charstring
  end interface vfile_mtime
  !
  ! vfile_normalize --
  !   Generic method  which returns the normalized filename
  !
  interface vfile_normalize
     module procedure vfile_normalize_vstring
     module procedure vfile_normalize_charstring
  end interface vfile_normalize
  !
  ! vfile_split --
  !   Generic method  which returns a list of components of the file
  !
  interface vfile_split
     module procedure vfile_split_vstring
     module procedure vfile_split_charstring
  end interface vfile_split
  !
  ! vfile_pathtype --
  !   Generic method  which returns the path type of the file
  !
  interface vfile_pathtype
     module procedure vfile_pathtype_vstring
     module procedure vfile_pathtype_charstring
  end interface vfile_pathtype
  !
  ! vfile_nativename --
  !   Generic method  which returns the native file name of the file
  !
  interface vfile_nativename
     module procedure vfile_nativename_vstring
     module procedure vfile_nativename_charstring
  end interface vfile_nativename
  !
  ! Static attributes
  !
  !
  ! Tags to manage errors.
  !
  integer , parameter, public :: VFILE_ERROR_OK = 0
  integer , parameter, public :: VFILE_ERROR_UNABLE_TO_OPEN_SOURCE = 1
  integer , parameter, public :: VFILE_ERROR_UNABLE_TO_OPEN_TARGET = 2
  integer , parameter, public :: VFILE_ERROR_UNABLE_TO_WRITE_TARGET = 3
  integer , parameter, public :: VFILE_ERROR_SOURCE_FILE_DOES_NOT_EXIST = 4
  integer , parameter, public :: VFILE_ERROR_UNDEFINED_SERVICE = 5
  integer , parameter, public :: VFILE_ERROR_UNABLE_TO_DELETE_DIRECTORY = 6
  integer , parameter, public :: VFILE_ERROR_UNABLE_TO_CHANGE_DIRECTORY = 7
  integer , parameter, public :: VFILE_ERROR_CURRENT_OBJECT_IS_NOT_DIRECTORY = 8
  !
  ! Maximum number of columns in a text file
  !
  integer, parameter, public :: VFILE_MAX_COLUMNS_TEXT = 1000
  !
  ! Tags for path types
  !
  integer, parameter, public :: VFILE_PATHTYPE_ABSOLUTE = 1
  integer, parameter, public :: VFILE_PATHTYPE_RELATIVE = 2
  integer, parameter, public :: VFILE_PATHTYPE_VOLUMERELATIVE = 3
  !
  ! Constants for platform separators
  !
  character(len=1), parameter :: VFILE_PLATFORM_SEPARATOR_WINDOWS = "\"
  character(len=1), parameter :: VFILE_PLATFORM_SEPARATOR_UNIX = "/"
  character(len=1), parameter :: VFILE_PLATFORM_SEPARATOR_MAC = ":"
  character(len=1), parameter :: VFILE_SLASH = "/"
  !
  ! Set to true to stop whenever an error comes in the vstring component.
  !
  logical, save :: vfile_stoponerror = .true.
  !
  ! Separator for the platform
  !
  character (len=1), parameter :: VFILE_PLATFORM_SEPARATOR_UNKNOWN = "?"
  character (len=1), save :: VFILE_PLATFORM_SEPARATOR = VFILE_PLATFORM_SEPARATOR_UNKNOWN
  !
  ! Maximum number of characters in the buffer.
  !
  integer , parameter :: VFILE_BUFFER_SIZE = 1000
  !
  ! Static, Platform-specific commands
  !
  type ( t_vstring ), save :: command_ls
  type ( t_vstring ), save :: command_copy
  type ( t_vstring ), save :: command_mkdir
  type ( t_vstring ), save :: command_redirect
  type ( t_vstring ), save :: command_suppress_msg
  type ( t_vstring ), save :: command_touch
  type ( t_vstring ), save :: command_rmdir
  type ( t_vstring ), save :: command_rmdir_force
  !
  ! Set to .true. if the static attributes have allready been initialized
  !
  logical :: vfile_static_initialized = .false.
contains
  !
  ! vfile_startup --
  !   Initialize module internal state.
  ! Arguments:
  !   no argument
  !
  subroutine vfile_startup ( )
    integer :: platform
    type ( t_vstring ) :: message
    type ( t_vstring ) :: platform_string

    if (.NOT.vfile_static_initialized) then
       !
       ! 0. Get the current platform
       !
       platform = platform_get_platform ()
       platform_string = vstring_format ( platform )
       !
       ! 1. Initialize the platform-specific separator
       !
       ! Setup the separator depending on the platform
       select case ( platform )
       case ( PLATFORM_PLATFORM_WINDOWS )
          VFILE_PLATFORM_SEPARATOR = VFILE_PLATFORM_SEPARATOR_WINDOWS
       case ( PLATFORM_PLATFORM_UNIX )
          VFILE_PLATFORM_SEPARATOR = VFILE_PLATFORM_SEPARATOR_UNIX
       case ( PLATFORM_PLATFORM_MAC )
          VFILE_PLATFORM_SEPARATOR = VFILE_PLATFORM_SEPARATOR_MAC
       case default
          call vstring_new ( message )
          call vstring_append ( message , "Unknown separator for platform :" )
          call vstring_append ( message , platform_string )
          call vstring_append ( message , " in vfile_startup" )
          call vfile_error ( message = message )
          call vstring_free ( message )
          call vstring_free ( platform_string )
          return
       end select
       !
       ! 2. Initialize the platform-specific commands
       !
       select case ( platform )
       case ( PLATFORM_PLATFORM_WINDOWS )
          ! See http://en.wikipedia.org/wiki/List_of_DOS_commands
          call vstring_new ( command_ls , "dir /b" )
          call vstring_new ( command_copy , "copy" )
          call vstring_new ( command_mkdir , "mkdir" )
          call vstring_new ( command_redirect , ">" )
          call vstring_new ( command_suppress_msg , "2>nul" )
          call vstring_new ( command_touch , "" )
          call vstring_new ( command_rmdir , "rd" )
          call vstring_new ( command_rmdir_force , "rmdir /s /q" )
       case ( PLATFORM_PLATFORM_UNIX )
          ! See http://en.wikipedia.org/wiki/List_of_Unix_programs
          call vstring_new ( command_ls , "ls" )
          call vstring_new ( command_copy , "cp" )
          call vstring_new ( command_mkdir , "md" )
          call vstring_new ( command_redirect , ">" )
          call vstring_new ( command_suppress_msg , "2>/dev/null" )
          call vstring_new ( command_touch , "touch" )
          call vstring_new ( command_rmdir , "rm" )
          call vstring_new ( command_rmdir_force , "rm -r -f" )
       case default
          call vstring_new ( message )
          call vstring_append ( message , "Unknown commands for platform :" )
          call vstring_append ( message , platform_string )
          call vstring_append ( message , " in vfile_startup" )
          call vfile_error ( message = message )
          call vstring_free ( message )
          call vstring_free ( platform_string )
          return
       end select
       !
       ! Update the static flag
       !
       vfile_static_initialized = .true.
       !
       ! Clean-up
       !
       call vstring_free ( platform_string )
    endif
  end subroutine vfile_startup
  !
  ! vfile_shutdown --
  !   Shutdown module internal state.
  subroutine vfile_shutdown ( )
    call vstring_free ( command_ls )
    call vstring_free ( command_copy )
    call vstring_free ( command_mkdir )
    call vstring_free ( command_redirect )
    call vstring_free ( command_suppress_msg )
    call vstring_free ( command_touch )
    call vstring_free ( command_rmdir )
    call vstring_free ( command_rmdir_force )
  end subroutine vfile_shutdown
  !
  ! vfile_rootname_vstring --
  !   Return the name without the extension (if any)
  ! Result:
  !   The part of the name _before_ the last "." or the whole name
  !   if no "." is present
  ! Arguments:
  !   filename   Name of the file to be examined, with type t_vstring
  !
  function vfile_rootname_vstring ( filename ) result ( rootname )
    type ( t_vstring ) , intent(in) :: filename
    type ( t_vstring ) :: rootname
    integer                      :: kdot
    integer                      :: kseparator
    type ( t_vstring ) :: subpart
    kdot   = vstring_last ( filename , '.' )
    kseparator = vfile_last_separator_index ( filename )
    call vstring_new ( rootname )
    if ( kdot /= 0 .and. kdot > kseparator+1 ) then
       subpart = vstring_range ( filename , 1 , kdot-1 )
       call vstring_append ( rootname ,  subpart )
       call vstring_free ( subpart )
    else
       call vstring_append ( rootname , filename )
    endif
  end function vfile_rootname_vstring
  !
  ! vfile_rootname_charstring --
  !   Returns the part of the name _before_ the last "." or the whole name
  !   if no "." is present
  ! Arguments
  !   filename The name of the file to process, with type character(len=*)
  !
#define _TEMPLATE_ROUTINE_NAME_CHARSTRING vfile_rootname_charstring
#define _TEMPLATE_ROUTINE_NAME_VSTRING vfile_rootname_vstring
#define _TEMPLATE_ROUTINE_VALUE type(t_vstring)
#include "m_vfile_template_casttovstring1.f90"
#undef _TEMPLATE_ROUTINE_NAME_CHARSTRING
#undef _TEMPLATE_ROUTINE_NAME_VSTRING
#undef _TEMPLATE_ROUTINE_VALUE
  !
  ! vfile_extension_vstring --
  !   Return the extension (if any)
  ! Result:
  !   The part of the name _after_ and including the last "." or empty if none
  !   present
  ! Arguments
  !   filename The name of the file to process, with type t_vstring
  ! Example : 
  !   if filename is "declaration.txt", the file extension is ".txt".
  !
  function vfile_extension_vstring ( filename ) result ( extension )
    type ( t_vstring ) , intent(in) :: filename
    type ( t_vstring ) :: extension
    integer                      :: kdot
    integer                      :: kseparator
    integer :: length
    type ( t_vstring ) :: subpart
    kdot   = vstring_last ( filename , '.' )
    kseparator = vfile_last_separator_index ( filename )
    call vstring_new ( extension )
    if ( kdot /= 0 .and. kdot > kseparator+1 ) then
       length = vstring_length ( filename )
       subpart = vstring_range ( filename , kdot , length )
       call vstring_append ( extension ,  subpart )
       call vstring_free ( subpart )
    endif
  end function vfile_extension_vstring
  !
  ! vfile_extension_charstring --
  !   Return the extension (if any)
  ! Arguments
  !   filename The name of the file to process, with type character(len=*)
  !
#define _TEMPLATE_ROUTINE_NAME_CHARSTRING vfile_extension_charstring
#define _TEMPLATE_ROUTINE_NAME_VSTRING vfile_extension_vstring
#define _TEMPLATE_ROUTINE_VALUE type(t_vstring)
#include "m_vfile_template_casttovstring1.f90"
#undef _TEMPLATE_ROUTINE_NAME_CHARSTRING
#undef _TEMPLATE_ROUTINE_NAME_VSTRING
#undef _TEMPLATE_ROUTINE_VALUE
  !
  ! vfile_tail_vstring --
  !   Returns all of the characters in name after the last directory separator
  ! Arguments:
  !   filename   Name of the file to be examined, with type t_vstring
  ! Result:
  !   Returns all of the characters in name after the last directory separator
  !   If name contains no separators then returns name.
  !
  recursive function vfile_tail_vstring ( filename ) result ( filetail )
    type ( t_vstring ) , intent(in) :: filename
    type ( t_vstring ) :: filetail
    integer :: kseparator
    integer :: length
    type ( t_vstring ) :: subpart
    type ( t_vstring ) :: filename_subpart
    logical :: isvolume
    integer :: current_platform
    kseparator = vfile_last_separator_index ( filename )
    length = vstring_length ( filename )
    current_platform = platform_get_platform ()
    if (current_platform==PLATFORM_PLATFORM_WINDOWS) then
       isvolume = vstring_match ( filename , "[a-zA-Z]:*" )
    else
       isvolume = .false.
    endif
    if ( isvolume ) then
       !
       ! The special case on windows where the filename is begining
       ! with the volume name, e.g. "C:/" or "C:\" where the file tail is an empty string
       ! or "C:/foo/faa/" where the file tail is "faa".
       !
       if ( length > 3 ) then
          filename_subpart = vstring_range ( filename , 4 , length)
          filetail = vfile_tail_vstring ( filename_subpart )
          call vstring_free ( filename_subpart )
       else
          call vstring_new ( filetail )
       endif
    elseif ( kseparator==length .AND. length > 1) then
       !
       ! This is the special case where the file ends with a separator,
       ! e.g. /foo/faa/. In that case the file tail is "faa".
       !
       filename_subpart = vstring_range ( filename , 1 , length - 1)
       filetail = vfile_tail_vstring ( filename_subpart )
       call vstring_free ( filename_subpart )
    else
       !
       ! General case where the file does not end with a separator.
       !
       call vstring_new ( filetail )
       if ( kseparator == 0 ) then
          !
          ! There is no separator at all
          !
          call vstring_append ( filetail , filename )
       elseif ( kseparator+1 <= length ) then
          !
          ! There are characters between the separator and the last character
          !
          subpart = vstring_range ( filename , kseparator+1 , length )
          call vstring_append ( filetail ,  subpart )
          call vstring_free ( subpart )
       endif
    endif
  end function vfile_tail_vstring
  !
  ! vfile_tail_charstring --
  !   Returns all of the characters in name after the last directory separator
  ! Arguments:
  !   filename   Name of the file to be examined, with type character(len=*)
  !
#define _TEMPLATE_ROUTINE_NAME_CHARSTRING vfile_tail_charstring
#define _TEMPLATE_ROUTINE_NAME_VSTRING vfile_tail_vstring
#define _TEMPLATE_ROUTINE_VALUE type(t_vstring)
#include "m_vfile_template_casttovstring1.f90"
#undef _TEMPLATE_ROUTINE_NAME_CHARSTRING
#undef _TEMPLATE_ROUTINE_NAME_VSTRING
#undef _TEMPLATE_ROUTINE_VALUE
  !
  ! vfile_dirname_vstring --
  !   Return the directory (if any)
  ! Result:
  !   The part of the name _before_ the last directory separator
  ! Arguments:
  !   filename   Name of the file to be examined, with type t_vstring
  !
  function vfile_dirname_vstring ( filename ) result ( dirname )
    type ( t_vstring ) , intent(in) :: filename
    type ( t_vstring ) :: dirname
    integer                      :: kseparator
    type ( t_vstring ) :: subpart
    kseparator = vfile_last_separator_index ( filename )
    call vstring_new ( dirname )
    if ( kseparator > 1 ) then
       subpart = vstring_range ( filename , 1 , kseparator  - 1)
       call vstring_append ( dirname ,  subpart )
       call vstring_free ( subpart )
    endif
  end function vfile_dirname_vstring
  !
  ! vfile_dirname_charstring --
  !   Return the directory (if any)
  ! Result:
  !   The part of the name _before_ the last directory separator
  ! Arguments:
  !   filename   Name of the file to be examined, with type character(len=*)
  !
#define _TEMPLATE_ROUTINE_NAME_CHARSTRING vfile_dirname_charstring
#define _TEMPLATE_ROUTINE_NAME_VSTRING vfile_dirname_vstring
#define _TEMPLATE_ROUTINE_VALUE type(t_vstring)
#include "m_vfile_template_casttovstring1.f90"
#undef _TEMPLATE_ROUTINE_NAME_CHARSTRING
#undef _TEMPLATE_ROUTINE_NAME_VSTRING
#undef _TEMPLATE_ROUTINE_VALUE
  !
  ! vfile_first_separator_index_vstring --
  !   Returns the index of the last separator in the given filename
  !   or 0 if there is no separator in the given file name.
  ! Arguments:
  !   filename   Name of the file to be examined, with type t_vstring
  !
  function vfile_first_separator_index_vstring ( filename ) result ( sepindex )
    type ( t_vstring ), intent(in) :: filename
    integer :: sepindex
    type ( t_vstring ) :: separator
    !
    ! 1. Search for platform-speficic separator
    !
    separator = vfile_separator ()
    sepindex = vstring_first ( filename , separator )
    call vstring_free ( separator )
    !
    ! 2. If not found, search for canonical separator
    !
    if ( sepindex == 0 ) then
       separator = vfile_canonicalseparator ()
       sepindex = vstring_first ( filename , separator )
       call vstring_free ( separator )
    endif
  end function vfile_first_separator_index_vstring
  !
  ! vfile_first_separator_index_charstring --
  !   Returns the index of the last separator in the given filename
  !   or 0 if there is no separator in the given file name.
  ! Arguments:
  !   filename   Name of the file to be examined, with type character(len=*)
  !
  function vfile_first_separator_index_charstring ( filename ) result ( sepindex )
    character(len=*), intent(in) :: filename
    integer :: sepindex
    type ( t_vstring ) :: filename_vstring
    call vstring_new ( filename_vstring , filename )
    sepindex = vfile_first_separator_index_vstring ( filename_vstring )
    call vstring_free ( filename_vstring )
  end function vfile_first_separator_index_charstring
  !
  ! vfile_last_separator_index_vstring --
  !   Returns the index of the last separator in the given filename
  !   or 0 if there is no separator in the given file name.
  ! Arguments:
  !   filename   Name of the file to be examined, with type t_vstring
  !
  function vfile_last_separator_index_vstring ( filename ) result ( sepindex )
    type ( t_vstring ) , intent(in) :: filename
    integer :: sepindex
    type ( t_vstring ) :: separator
    !
    ! 1. Search for platform-speficic separator
    !
    separator = vfile_separator ()
    sepindex = vstring_last ( filename , separator )
    call vstring_free ( separator )
    !
    ! 2. If not found, search for canonical separator
    !
    if ( sepindex == 0 ) then
       separator = vfile_canonicalseparator ()
       sepindex = vstring_last ( filename , separator )
       call vstring_free ( separator )
    endif
  end function vfile_last_separator_index_vstring
  !
  ! vfile_last_separator_index_charstring --
  !   Returns the index of the last separator in the given filename
  !   or 0 if there is no separator in the given file name.
  ! Arguments:
  !   filename   Name of the file to be examined, with type character(len=*)
  !
  function vfile_last_separator_index_charstring ( filename ) result ( sepindex )
    character(len=*), intent(in) :: filename
    integer :: sepindex
    type ( t_vstring ) :: filename_vstring
    call vstring_new ( filename_vstring , filename )
    sepindex = vfile_last_separator_index_vstring ( filename_vstring )
    call vstring_free ( filename_vstring )
  end function vfile_last_separator_index_charstring
  !
  ! vfile_join_vstring --
  !   Join the current file with the given file name,
  !   using the platform-specific separator as the joining character.
  !   The result is always canonical for the current platform: / for 
  !   Unix and Windows, and : for Macintosh.
  !   If a particular name is relative, then it will be joined to the previous 
  !   file name argument. Otherwise, any earlier arguments will be discarded, 
  !   and joining will proceed from the current argument. For example,
  !     vfile_join ( a , b , /foo , bar )
  !   returns /foo/bar.
  ! Arguments:
  !   dirname Name of the directory
  !   filename   Name of the file to join at the end of the directory, with type t_vstring
  !
  function vfile_join_vstring ( dirname , filename ) result ( fullname )
    type ( t_vstring ) , intent(in) :: dirname
    type ( t_vstring ) , intent(in)  :: filename
    type ( t_vstring ) :: fullname
    type ( t_vstring ) :: separator
    integer :: lastsep
    integer :: dirname_length
    integer :: pathtype
    call vstring_new ( fullname )
    !
    ! 1. Add the directory name
    !
    call vstring_append ( fullname , dirname )
    !
    ! If the file type is absolute, remove the directory
    !
    pathtype = vfile_pathtype ( filename )
    if ( pathtype == VFILE_PATHTYPE_ABSOLUTE .OR. &
         pathtype == VFILE_PATHTYPE_VOLUMERELATIVE) then
       call vstring_free ( fullname )
       call vstring_new ( fullname , filename )
    else
       !
       ! If the last character of the directory name is allready
       ! a separator, do not insert a separator.
       ! Otherwise, insert one.
       !
       lastsep = vfile_last_separator_index ( dirname )
       dirname_length = vstring_length ( dirname )
       if ( lastsep /= dirname_length ) then
          separator = vfile_canonicalseparator ()
          call vstring_append ( fullname , separator )
          call vstring_free ( separator )
       endif
       call vstring_append ( fullname , filename )
    endif
  end function vfile_join_vstring
  !
  ! vfile_join_charstring --
  !   Join the current file with the given file name,
  !   using the platform-specific separator as the joining character.
  ! Arguments:
  !   dirname Name of the directory
  !   filename   Name of the file to join at the end of the directory, with type character(len=*)
  !
#define _TEMPLATE_ROUTINE_NAME_CHARSTRING vfile_join_charstring
#define _TEMPLATE_ROUTINE_NAME_VSTRING vfile_join_vstring
#include "m_vfile_template_casttovstring2.f90"
#undef _TEMPLATE_ROUTINE_NAME_CHARSTRING
#undef _TEMPLATE_ROUTINE_NAME_VSTRING
  !
  ! vfile_add_extension_vstring --
  !   Return a new file name with the given extension concatenated.
  !   If the given file name ends with a dot and the given extension begins
  !   with a dot, only one dot is kept.
  ! Arguments:
  !   filename   Name of the file to be used
  !   extension  Extension to be added (e.g. ".txt"), with type t_vstring
  ! Result:
  !   The file name with an added extension
  ! Note :
  !   The extension of one file begins with a dot : ".txt" is a file
  !   extension while "txt" is not.
  !
  function vfile_add_extension_vstring ( filename , extension ) result ( newname )
    type ( t_vstring ) , intent(in) :: filename
    type ( t_vstring ) , intent(in) :: extension
    type ( t_vstring ) :: newname
    integer :: kdot
    integer :: length
    type ( t_vstring ) :: subpart
    kdot    = vstring_first ( filename , "." )
    length = vstring_length ( filename )
    call vstring_new ( newname )
    if ( kdot == length ) then
       ! The file name ends with a dot, so we do not add one.
       subpart = vstring_range ( filename , 1 , kdot-1 )
       call vstring_append ( newname ,  subpart )
       call vstring_free ( subpart )
       call vstring_append ( newname , extension )
    else
       call vstring_append ( newname , filename )
       call vstring_append ( newname , extension )
    endif
  end function vfile_add_extension_vstring
  !
  ! vfile_add_extension_charstring --
  !   Return a new file name with the given extension concatenated.
  !   If the given file name ends with a dot and the given extension begins
  !   with a dot, only one dot is kept.
  ! Arguments:
  !   filename   Name of the file to be used
  !   extension  Extension to be added (e.g. ".txt"), with type character(len=*)
  !
#define _TEMPLATE_ROUTINE_NAME_CHARSTRING vfile_add_extension_charstring
#define _TEMPLATE_ROUTINE_NAME_VSTRING vfile_add_extension_vstring
#include "m_vfile_template_casttovstring2.f90"
#undef _TEMPLATE_ROUTINE_NAME_CHARSTRING
#undef _TEMPLATE_ROUTINE_NAME_VSTRING
  !
  ! vfile_separator --
  !   Return the native separator for the current platform
  !   The separator depends on the platform :
  !   - "/" on Unix, Linux systems,
  !   - "\" on Windows systems,
  !   - ":" on Macintosh.
  !
  function vfile_separator ( )
    type ( t_vstring ) :: vfile_separator
    call vstring_new ( vfile_separator )
    call vstring_append ( vfile_separator , VFILE_PLATFORM_SEPARATOR )
  end function vfile_separator
  !
  ! vfile_canonicalseparator --
  !   Return the canonical separator for the current platform :
  !   - Windows, Unix : "/"
  !   - Mac : ":"
  !
  function vfile_canonicalseparator ( )
    type ( t_vstring ) :: vfile_canonicalseparator
    integer :: platform
    type ( t_vstring ) :: message
    type ( t_vstring ) :: platform_string
    platform = platform_get_platform ()
    select case ( platform )
    case ( PLATFORM_PLATFORM_WINDOWS )
       call vstring_new ( vfile_canonicalseparator , VFILE_SLASH )

    case ( PLATFORM_PLATFORM_UNIX )
       call vstring_new ( vfile_canonicalseparator , VFILE_SLASH )

    case ( PLATFORM_PLATFORM_MAC )
       call vstring_new ( vfile_canonicalseparator , VFILE_PLATFORM_SEPARATOR_MAC )

    case default
       platform_string = vstrplatform_platformstring ( )
       call vstring_new ( message )
       call vstring_append ( message , "Unknown canonical separator for platform :" )
       call vstring_append ( message , platform_string )
       call vstring_append ( message , " in vfile_canonicalseparator" )
       call vfile_error ( message = message )
       call vstring_free ( message )
       call vstring_free ( platform_string )
       return
    end select
  end function vfile_canonicalseparator
  !
  ! vfile_pwd --
  !   Returns the name of the current directory by using the fortran
  !   extension GETCWD.
  !   The separator used here is the platform-independent "/".
  ! Arguments:
  !   cwd : the current working directory
  !
  subroutine vfile_pwd ( pwd , status )
    type ( t_vstring ) :: pwd
    integer , intent(out) , optional :: status
    ! TODO : how to remove that limitation that the number of characters is fixed ?
    character(len=VFILE_BUFFER_SIZE) :: cwd
    type ( t_vstring ) :: pwd_string
    type ( t_vstring ) :: pwd_trim
    integer :: local_status
    type ( t_vstring ) :: message
#ifdef _VFILE_GETCWD_FUNCTION
    local_status = GETCWD ( cwd )
#endif
#ifdef _VFILE_GETCWD_SUBROUTINE
    call GETCWD ( cwd , local_status )
#endif
#ifndef _VFILE_GETCWD_FUNCTION
#ifndef _VFILE_GETCWD_SUBROUTINE
    !
    ! Case when no macro is defined
    !
    if ( present ( status ) ) then
       status = VFILE_ERROR_UNDEFINED_SERVICE
       return
    else
       call vstring_new ( message , "The pwd service is not provided in vfile_pwd" )
       call vfile_error ( message = message )
       call vstring_free ( message )
       return
    endif
#endif
#endif
    !
    ! Process errors.
    !
    if ( present ( status )) then
       status = local_status
    elseif ( local_status /=0 ) then
       call vstring_new ( message , "Error while getting current working directory in vfile_pwd" )
       call vfile_error ( message = message )
       call vstring_free ( message )
       return
    endif
    !
    ! Trim the working directory and update the separator if necessary.
    !
    call vstring_new ( pwd_string , cwd )
    pwd_trim = vstring_trim ( pwd_string )
    pwd = vfile_changesep ( pwd_trim )
    call vstring_free ( pwd_string )
    call vstring_free ( pwd_trim )
  end subroutine vfile_pwd
  !
  ! vfile_changesep --
  !   Replace the platform-specific separator in the given file name
  !   into and returns the result file name.
  !   If the separator is not provided, use the canonical separator as the replacement separator.
  !   If the separator is provided, use it as the replacement separator.
  !
  function vfile_changesep ( filename , separator )
    type ( t_vstring ) :: filename
    type ( t_vstring ) , optional :: separator
    type ( t_vstring ) :: vfile_changesep
    integer :: nbchar
    integer :: charindex
    type ( t_vstring ) :: separator_real
    type ( t_vstring ) :: currentchar
    logical :: equalssep1
    logical :: equalssep2
    type ( t_vstring ) :: platformseparator
    type ( t_vstring ) :: canonicalseparator
    !
    ! Process options
    !
    if ( present ( separator ) ) then
       call vstring_new ( separator_real , separator )
    else
       separator_real = vfile_canonicalseparator ( )
    endif
    !
    ! Do the replacement
    !
    platformseparator = vfile_separator ( )
    canonicalseparator = vfile_canonicalseparator ( )
    nbchar = vstring_length ( filename )
    call vstring_new ( vfile_changesep )
    do charindex = 1 , nbchar
       currentchar = vstring_index ( filename , charindex )
       equalssep1 = vstring_equals ( currentchar , platformseparator )
       equalssep2 = vstring_equals ( currentchar , canonicalseparator )
       if ( equalssep1 .OR. equalssep2 ) then
          call vstring_append ( vfile_changesep , separator_real )
       else
          call vstring_append ( vfile_changesep , currentchar )
       endif
       call vstring_free ( currentchar )
    enddo
    !
    ! Clean-up
    !
    call vstring_free ( platformseparator )
    call vstring_free ( canonicalseparator )
    call vstring_free ( separator_real )
  end function vfile_changesep
  !
  ! vfile_exists_vstring --
  !   Returns .true. if file name exists, .false. otherwise.
  ! Arguments:
  !   filename   Name of the file to be examined, with type t_vstring
  !
  function vfile_exists_vstring ( filename ) result ( exists )
    type ( t_vstring ) , intent(in) :: filename
    logical :: exists
    logical :: fexist
#ifdef _VFILE_STATIC_BUFFER
    character ( len = VFILE_BUFFER_SIZE ) :: filename_charstring
#else
    character ( len = vstring_length ( filename )) :: filename_charstring
#endif
    call vstring_cast ( filename , filename_charstring )
    ! Note :
    ! The other possibility is the ACCESS fortran extension
    ! But the "inquire" intrinsic in fortran standard.
    inquire ( FILE = filename_charstring , EXIST = fexist )
    if (fexist) then
       exists = .true.
    else
       exists = .false.
    endif
  end function vfile_exists_vstring
  !
  ! vfile_exists_charstring --
  !   Returns .true. if file name exists, .false. otherwise.
  ! Arguments:
  !   filename   Name of the file to be examined, with type character(len=*)
  !
  function vfile_exists_charstring ( filename ) result ( exists )
    character(len=*) , intent(in) :: filename
    logical :: exists
    type ( t_vstring ) :: filename_vstring
    call vstring_new ( filename_vstring , filename )
    exists = vfile_exists_vstring ( filename_vstring )
    call vstring_free ( filename_vstring )
  end function vfile_exists_charstring
  !
  ! vfile_rename_vstring --
  !   Renames the file ofdln to newfn by using the RENAME fortran extension.
  ! Arguments:
  !   filename : the current file
  !   newfn : the new file name, with type t_vstring
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !     upon return
  !
  subroutine vfile_rename_vstring ( filename , newfn , status )
    type ( t_vstring ), intent(in) :: filename
    type ( t_vstring ), intent(in) :: newfn
    integer, intent(out) , optional :: status
    integer :: local_status
    type ( t_vstring ) :: message
    logical :: fexist
#ifdef _VFILE_STATIC_BUFFER
    character ( len = VFILE_BUFFER_SIZE ) :: filename_charstring
    character ( len = VFILE_BUFFER_SIZE ) :: newfn_charstring
#else
    character ( len = vstring_length ( filename )) :: filename_charstring
    character ( len = vstring_length(newfn)) :: newfn_charstring
#endif
    !
    ! Check source file.
    !
    fexist = vfile_exists ( filename )
    if ( .NOT. fexist ) then
       if ( present ( status )) then
          status = VFILE_ERROR_SOURCE_FILE_DOES_NOT_EXIST
          return
       else
          call vstring_new ( message )
          call vstring_append ( message , "Unable to access to current file :" )
          call vstring_append ( message , filename )
          call vstring_append ( message , " in vfile_rename" )
          call vfile_error ( filename , message )
          call vstring_free ( message )
          return
       endif
    endif
    !
    ! Rename the file with a fortran extension
    !
    call vstring_cast ( filename , filename_charstring )
    call vstring_cast ( newfn , newfn_charstring )
#ifdef _VFILE_RENAME_FUNCTION
    local_status = RENAME ( filename_charstring , newfn_charstring )
#endif
#ifdef _VFILE_RENAME_SUBROUTINE
    call RENAME ( filename_charstring , newfn_charstring , local_status )
#endif
#ifndef _VFILE_RENAME_FUNCTION
#ifndef _VFILE_RENAME_SUBROUTINE
    !
    ! Case when no macro is defined
    !
    if ( present ( status ) ) then
       status = VFILE_ERROR_UNDEFINED_SERVICE
       return
    else
       call vstring_new ( message , "The rename service is not provided in vfile_rename" )
       call vfile_error ( filename , message )
       call vstring_free ( message )
       return
    endif
#endif
#endif
    !
    ! Process errors
    !
    if ( present ( status )) then
       status = local_status
    elseif ( local_status /=0 ) then
       call vstring_new ( message )
       call vstring_append ( message , "Unable to rename file " )
       call vstring_append ( message , filename )
       call vstring_append ( message , " to file " )
       call vstring_append ( message , newfn )
       call vstring_append ( message , " in vfile_rename" )
       call vfile_error ( filename , message )
       call vstring_free ( message )
    endif
  end subroutine vfile_rename_vstring
  !
  ! vfile_rename_charstring --
  !   Renames the file ofdln to newfn by using the RENAME fortran extension.
  ! Arguments:
  !   filename : the current file
  !   newfn : the new file name, with type character(len=*)
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !     upon return
  !
  subroutine vfile_rename_charstring ( filename , newfn , status )
    type ( t_vstring ), intent(in) :: filename
    character(len=*), intent(in) :: newfn
    integer, intent(out) , optional :: status
    type ( t_vstring ) :: newfn_vstring
    call vstring_new ( newfn_vstring , newfn )
    call vfile_rename_vstring ( filename , newfn_vstring , status )
    call vstring_free ( newfn_vstring )
  end subroutine vfile_rename_charstring
  !
  ! vfile_copy_vstring --
  !   Copy the ascii file ofdln to targetfn.
  !   If the source file does not exists, generates an error.
  !   If the target file allready exists and force option is undefined
  !   or defined to false, generates an error.
  ! Arguments:
  !   filename : the current file name
  !   targetfn : the target file name
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !     upon return
  !     status = 1 when one was unable to open the source file
  !     status = 2 when one was unable to open the target file
  !     status = 3 when there was a problem while writing the target file
  !     status = 4 when the source file does not exist
  !   mode, option : if not supplied or supplied and equals to "system", then 
  !     the copy is made using an operating system command.
  !     If supplied and equals to "ascii", then the copy is made using standard
  !     fortran.
  !   force, optional : if supplied and true, if the target file allready exists, delete it before
  !     making the copy. filename option is available only in "system" mode.
  ! Caution !
  !   1. The maximum number of columns in the source filename is 1000.
  !   2. After execution, the target file is not an exact copy of the source file.
  !   Because of the fortran format used, all the lines of the target file are of length 1000 :
  !   blank spaces are appended at the end of the string.
  !
  subroutine vfile_copy_vstring ( filename , targetfn , status , mode , force , trimline )
    type ( t_vstring ), intent(in) :: filename
    type ( t_vstring ), intent(in) :: targetfn
    integer, intent(out) , optional :: status
    logical, intent(in) , optional :: force
    logical, intent(in), optional :: trimline
    character(len=*), intent(in), optional :: mode
    character(len=10) :: mode_real
    type ( t_vstring ) :: message
    !
    ! Process options
    !
    if ( present ( mode ) ) then
       mode_real = mode
    else
       mode_real = "system"
    endif
    !
    ! Make the copy
    !
    select case ( mode_real )
    case ("system")
       call vfile_copy_system ( filename , targetfn , status )
    case ("ascii")
       call vfile_copy_ascii ( filename , targetfn , status , force , trimline )
    case default
       call vstring_new ( message )
       call vstring_append ( message , "Unknown mode :" )
       call vstring_append ( message , mode )
       call vstring_append ( message , " in vfile_copy" )
       call vfile_error ( message = message )
       call vstring_free ( message )
    end select
  end subroutine vfile_copy_vstring
  !
  ! vfile_copy_charstring --
  !   Copy the ascii file ofdln to targetfn.
  !   If the source file does not exists, generates an error.
  !   If the target file allready exists and force option is undefined
  !   or defined to false, generates an error.
  ! Arguments:
  !   filename : the current file name
  !   targetfn : the target file name with type character(len=*)
  !
  subroutine vfile_copy_charstring ( filename , targetfn , status , mode , force , trimline )
    type ( t_vstring ), intent(in) :: filename
    character(len=*), intent(in) :: targetfn
    integer, intent(out) , optional :: status
    logical, intent(in) , optional :: force
    logical, intent(in), optional :: trimline
    character(len=*), intent(in), optional :: mode
    type ( t_vstring ) :: targetfn_vstring
    call vstring_new ( targetfn_vstring , targetfn )
    call vfile_copy_vstring ( filename , targetfn_vstring , status , mode , force , trimline )
    call vstring_free ( targetfn_vstring )
  end subroutine vfile_copy_charstring
  !
  ! vfile_copy_system --
  !   Copy the file ofdln to newfn by using the SYSTEM fortran extension.
  ! Arguments:
  !   filename : the current file name
  !   targetfn : the target file name
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !      upon return
  ! Note :
  !    filename subroutine is system-dependent.
  !
  subroutine vfile_copy_system ( filename , targetfn , status )
    type ( t_vstring ), intent(in) :: filename
    type ( t_vstring ), intent(in) :: targetfn
    integer, intent(out) , optional :: status
    integer :: local_status
    type ( t_vstring ) :: command
    type ( t_vstring ) :: message
    logical :: fexist
    type ( t_vstring ) :: filename_native
    type ( t_vstring ) :: targetfn_native
    !
    ! Check source file.
    !
    fexist = vfile_exists ( filename )
    if ( .NOT. fexist ) then
       if ( present ( status )) then
          status = VFILE_ERROR_SOURCE_FILE_DOES_NOT_EXIST
       else
       call vstring_new ( message )
       call vstring_append ( message , "Unable to access to curent file :" )
       call vstring_append ( message , filename )
       call vstring_append ( message , " in vfile_copy_system" )
       call vfile_error ( filename , message )
       call vstring_free ( message )
       endif
       return
    endif
    !
    ! Make the copy
    !
    !
    ! Convert the file name to native name to make the platform-specific
    ! command work.
    !
    filename_native = vfile_nativename ( filename )
    targetfn_native = vfile_nativename ( targetfn )
    call vstring_new ( command )
    call vstring_append ( command , command_copy )
    call vstring_append ( command , " " )
    call vstring_append ( command , filename_native )
    call vstring_append ( command , " " )
    call vstring_append ( command , targetfn_native )
    call vstrplatform_system ( command , local_status )
    call vstring_free ( command )
    call vstring_free ( filename_native )
    call vstring_free ( targetfn_native )
    if ( present ( status )) then
       status = local_status
    elseif ( local_status /=0 ) then
       call vstring_new ( message , "Unable to copy :" )
       call vstring_append ( message , filename )
       call vstring_append ( message , " into " )
       call vstring_append ( message , targetfn )
       call vstring_append ( message , " in vfile_copy_system" )
       call vfile_error ( filename , message )
       call vstring_free ( message )
    endif
  end subroutine vfile_copy_system
  !
  ! vfile_copy_ascii --
  !   Copy the ascii file ofdln to targetfn by using standard fortran.
  !   If the source file does not exists, generates an error.
  !   If the target file allready exists and force option is undefined
  !   or defined to false, generates an error.
  ! Arguments:
  !   sourcefn : the source file name
  !   targetfn : the target file name
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !     upon return
  !     status = 1 when one was unable to open the source file
  !     status = 2 when one was unable to open the target file
  !     status = 3 when there was a problem while writing the target file
  !     status = 4 when the source file does not exist
  !   force, optional : if supplied and true, if the target file allready exists, delete it before
  !                making the copy.
  !   trimline, optional : if supplied and true, or not supplied, the lines of 
  !     the file copy are trimmed.
  !     If supplied and false, the number of columns in the file copy are all 
  !     of maximum possible length.
  ! Caution !
  !   1. The maximum number of columns in the source filename is 1000.
  !   2. After execution, the target file is not an exact copy of the source file.
  !   Because of the fortran format used, all the lines of the target file are of length 1000 :
  !   blank spaces are appended at the end of the string.
  !
  !
  subroutine vfile_copy_ascii ( filename , targetfn , status , force , trimline )
    type ( t_vstring ), intent(in) :: filename
    type ( t_vstring ), intent(in) :: targetfn
    integer, intent(out) , optional :: status
    logical, intent(in) , optional :: force
    logical, intent(in), optional :: trimline
    integer :: local_status
    integer :: source_unit, target_unit
    character ( len = VFILE_MAX_COLUMNS_TEXT ) :: string
    integer :: end_of_file
    logical :: fexist
    type ( t_vstring ) :: message
    logical :: trimline_real
    !
    ! Process options
    !
    if ( present ( trimline ) ) then 
       trimline_real = trimline
    else
       trimline_real = .true.
    endif
    !
    ! By default, there is no problem.
    !
    local_status = VFILE_ERROR_OK
    !
    ! 0. Check that the source file exist.
    !
    fexist = vfile_exists ( filename )
    if ( .NOT. fexist ) then
       if ( present ( status )) then
          status = VFILE_ERROR_SOURCE_FILE_DOES_NOT_EXIST
       else
          call vstring_new ( message )
          call vstring_append ( message , "Source file :" )
          call vstring_append ( message , filename )
          call vstring_append ( message , " does not exist in vfile_copy_ascii" )
          call vfile_error ( filename , message )
          call vstring_free ( message )
       endif
       return
    endif
    !
    ! 1. If the target file exists, delete it if force = .true.
    !
    if (present ( force )) then
       if (force) then
          fexist = vfile_exists ( targetfn )
          if (fexist) then
             call vfile_delete ( targetfn )
          endif
       endif
    endif
    !
    ! 2. Process the copy
    !
    source_unit = vfile_open ( filename , action='read',status='old', iostat= local_status )
    if ( local_status /=0) then
       local_status = VFILE_ERROR_UNABLE_TO_OPEN_SOURCE
    else
       target_unit = vfile_open ( targetfn , action='write',status='new', iostat= local_status )
       if ( local_status /=0) then
          local_status = VFILE_ERROR_UNABLE_TO_OPEN_TARGET
       else
          !
          ! 3. Copy the lines one after another.
          !
          do
             read ( source_unit , '(a)', iostat = end_of_file ) string
             if ( end_of_file /= 0 ) then
                ! The last line has been found.
                exit
             end if
             if ( trimline_real ) then
                write ( target_unit , '(a)', iostat = local_status ) trim(string)
             else
                write ( target_unit , '(a)', iostat = local_status ) string
             endif
             if ( local_status /= 0 ) then
                ! There was an error while writing
                local_status = VFILE_ERROR_UNABLE_TO_WRITE_TARGET
                exit
             end if
          enddo
          close ( target_unit )
       endif
       close ( source_unit )
    endif
    !
    ! Process status
    !
    if ( present ( status )) then
       status = local_status
    elseif ( local_status /=0 ) then
       call vstring_new ( message )
       call vstring_append ( message , "Unable to copy :" )
       call vstring_append ( message , filename )
       call vstring_append ( message , " into " )
       call vstring_append ( message , targetfn )
       call vstring_append ( message , " in vfile_copy_ascii" )
       call vfile_error ( filename , message )
       call vstring_free ( message )
    endif
  end subroutine vfile_copy_ascii
  !
  ! vfile_delete_vstring --
  !   Removes the file or directory specified by each pathname argument. 
  !   Non-empty directories will be removed only if the force option is specified.
  ! Arguments:
  !   filename   Name of the file to be examined
  !   force, optional : if supplied and true, forces to delete the file, even if it is empty.
  !     If not supplied or supplied and false, the file is not deleted if it is empty.
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !     upon return
  !
  subroutine vfile_delete_vstring ( filename , force , status )
    type ( t_vstring ), intent(in) :: filename
    logical, intent (in) ,optional :: force
    integer, intent (out), optional :: status
    integer :: local_status
    integer :: file_unit
    type ( t_vstring ) :: message
    logical :: isdir
    logical :: force_real
    type ( t_vstring ) :: command
    type ( t_vstring ) :: filename_native
    !
    ! Process options
    !
    if ( present ( force ) ) then
       force_real = force
    else
       force_real = .false.
    endif
    isdir = vfile_isdirectory ( filename )
    if (isdir) then
       !
       ! Delete that directory
       !
       call vstring_new ( command )
       if (force_real) then
          call vstring_append ( command , command_rmdir_force )
       else
          call vstring_append ( command , command_rmdir )
       endif
       !
       ! Convert the file name to native name to make the platform-specific
       ! command work.
       !
       filename_native = vfile_nativename ( filename )
       call vstring_append ( command , " " )
       call vstring_append ( command , filename_native )
       call vstrplatform_system ( command , local_status )
       call vstring_free ( command )
       call vstring_free ( filename_native )
       isdir = vfile_isdirectory ( filename )
       if (isdir) then
          local_status = VFILE_ERROR_UNABLE_TO_DELETE_DIRECTORY
       endif
    else
       ! Delete that regular file
       ! One could use the following fortran extension :
       !   call UNLINK ( filename , local_status )
       ! But the following code uses standard fortran statements and therefore
       ! will be full portable.
       file_unit = vfile_open ( filename , status ='old', iostat= local_status )
       if ( local_status == 0 ) then
          close ( UNIT = file_unit , STATUS = 'DELETE', IOSTAT= local_status )
       endif
    endif
    !
    ! Process status
    !
    if (present ( status )) then
       status = local_status
    elseif ( local_status /=0 ) then
       call vstring_new ( message )
       call vstring_append ( message , "Unable to delete file :" )
       call vstring_append ( message , filename )
       call vstring_append ( message , " in vfile_delete" )
       call vfile_error ( filename , message )
       call vstring_free ( message )
    endif
  end subroutine vfile_delete_vstring
  !
  ! vfile_delete_charstring --
  !   Removes the file or directory specified by each pathname argument. 
  !   Non-empty directories will be removed only if the force option is specified.
  !
  subroutine vfile_delete_charstring ( filename , force , status )
    character(len=*), intent(in) :: filename
    logical, intent (in) ,optional :: force
    integer, intent (out), optional :: status
    type ( t_vstring ) :: filename_vstring
    call vstring_new ( filename_vstring , filename )
    call vfile_delete_vstring ( filename_vstring , force , status )
    call vstring_free ( filename_vstring )
  end subroutine vfile_delete_charstring
  !
  ! vfile_isdirectory_vstring --
  !   Returns .true. if file name is a directory, .false. otherwise.
  !
  function vfile_isdirectory_vstring ( filename ) result ( isdirectory )
    type ( t_vstring ), intent(in) :: filename
    logical :: isdirectory
    type ( t_vstring ) :: cwd
    integer :: status
    call vfile_pwd (cwd)
    call vstrplatform_cd ( filename , status )
    if ( status == 0 ) then
       isdirectory = .true.
    else
       isdirectory = .false.
    endif
    call vstrplatform_cd ( cwd )
    call vstring_free ( cwd )
  end function vfile_isdirectory_vstring
  !
  ! vfile_isdirectory_charstring --
  !   Returns .true. if file name is a directory, .false. otherwise.
  !
  function vfile_isdirectory_charstring ( filename ) result ( isdirectory )
    character(len=*), intent(in) :: filename
    logical :: isdirectory
    type ( t_vstring ) :: filename_vstring
    call vstring_new ( filename_vstring , filename )
    isdirectory = vfile_isdirectory_vstring ( filename_vstring )
    call vstring_free ( filename_vstring )
  end function vfile_isdirectory_charstring
  !
  ! vfile_atime_vstring --
  !   Returns an integer representing the time at which file name was last accessed.
  ! Arguments:
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !     upon return
  !
  function vfile_atime_vstring ( filename , status ) result ( vfile_atime )
    type ( t_vstring ), intent(in) :: filename
    integer, dimension (1:13) :: statarray
    integer, intent(out) , optional :: status
    integer :: vfile_atime
    integer  :: local_status
    call vstrplatform_stat ( filename , statarray , local_status )
    vfile_atime = statarray (9)
    if (present ( status )) then
       status = local_status
    endif
  end function vfile_atime_vstring
  !
  ! vfile_atime_charstring --
  !   Returns an integer representing the time at which file name was last accessed.
  !
  function vfile_atime_charstring ( filename , status ) result ( vfile_atime )
    character(len=*), intent(in) :: filename
    integer, intent(out) , optional :: status
    integer :: vfile_atime
    type ( t_vstring ) :: filename_vstring
    call vstring_new ( filename_vstring , filename )
    vfile_atime = vfile_atime_vstring ( filename_vstring , status )
    call vstring_free ( filename_vstring )
  end function vfile_atime_charstring
  !
  ! vfile_mtime_vstring --
  !   Returns an integer representing the time at which file name was last modified.
  ! Arguments:
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !     upon return
  !
  function vfile_mtime_vstring ( filename , status ) result ( vfile_mtime )
    type ( t_vstring ), intent(in) :: filename
    integer, dimension (1:13) :: statarray
    integer, intent(out) , optional :: status
    integer :: vfile_mtime
    integer  :: local_status
    call vstrplatform_stat ( filename , statarray , local_status )
    vfile_mtime = statarray (10)
    if (present ( status )) then
       status = local_status
    endif
  end function vfile_mtime_vstring
  !
  ! vfile_mtime_charstring --
  !   Returns an integer representing the time at which file name was last modified.
  ! Arguments:
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !     upon return
  !
  function vfile_mtime_charstring ( filename , status ) result ( vfile_mtime )
    character(len=*), intent(in) :: filename
    integer, intent(out) , optional :: status
    integer :: vfile_mtime
    type ( t_vstring ) :: filename_vstring
    call vstring_new ( filename_vstring , filename )
    vfile_mtime = vfile_mtime_vstring ( filename_vstring , status )
    call vstring_free ( filename_vstring )
  end function vfile_mtime_charstring
  !
  ! vfile_normalize_vstring --
  !   Returns a unique normalized path representation for the
  !   file-system object (file, directory, link, etc), whose string
  !   value can be used as a unique identifier for it. A normalized path
  !   is an absolute path which has all '../', './' removed. Also it is one which
  !   is in the ``standard'' format for the native platform.
  !   On Windows or Mac, any platform-specific separator in the path
  !   is replaced by the platform-independent separator "/".
  !   On Windows it also means we want the long form with that form's 
  !   case-dependence (which gives us a unique, case-dependent path).
  ! Arguments:
  !   normalized  The file name to normalize, with type t_vstring
  !
  function vfile_normalize_vstring ( filename ) result ( vfile_normalize )
    type ( t_vstring ), intent(in) :: filename
    type ( t_vstring ) :: vfile_normalize
    type ( t_vstring ) :: directory
    type ( t_vstring ) :: absolutename
    type ( t_vstringlist ) :: listOfFiles
    integer :: pathtype
    logical , dimension (:) , allocatable :: keepitem
    integer :: nbitems
    integer :: itemindex
    type ( t_vstring ) :: item
    logical :: isdotdot
    logical :: isdot
    integer :: skipnb
    type ( t_vstring ) :: separator
    integer :: pathindex
    type ( t_vstring ) :: cwd
    integer :: firstsep
    type ( t_vstring ) :: filevolume
    type ( t_vstring ) :: fileend
    integer :: filelength
    logical :: isdirectory
    !
    ! Compute an absolute path name : absolutename
    !
    pathtype = vfile_pathtype ( filename )
    if ( pathtype == VFILE_PATHTYPE_ABSOLUTE ) then
       call vstring_new ( absolutename , filename )
    elseif ( pathtype == VFILE_PATHTYPE_VOLUMERELATIVE ) then
       !
       ! Replace the leading / by the current volume name
       !
       call vfile_pwd (cwd)
       firstsep = vfile_first_separator_index ( cwd )
       filevolume = vstring_range ( cwd , 1 , firstsep - 1 )
       ! Remove the leading / in the filename
       filelength = vstring_length ( filename )
       if ( filelength > 1 ) then
          fileend = vstring_range ( filename , 2 , filelength )
          absolutename = vfile_join ( filevolume , fileend )
          call vstring_free ( fileend )
       else
          call vstring_new ( absolutename , filevolume )
       endif
       call vstring_free ( cwd )
       call vstring_free ( filevolume )
    else
       !
       ! Add the current directory to the relative path
       !
       call vfile_pwd (directory)
       absolutename = vfile_join ( directory , filename )
       call vstring_free ( directory )
    endif
    !
    ! 2. Splitting the absolutename into pieces
    !
    listOfFiles = vfile_split ( absolutename )
    call vstring_free ( absolutename )
    nbitems = vstrlist_length ( listOfFiles )
    !
    ! 3. Tag the components in the path which must be kept
    !
    ! By default all items must be kept in the path
    allocate ( keepitem (1:nbitems) )
    keepitem = .true.
    ! skipnb is the current number of items to skip
    skipnb = 0
    !
    ! 3.1 Process a loop from the end of the path to the begining
    !
    do itemindex = nbitems , 1 , -1
       item = vstrlist_index ( listOfFiles , itemindex )
       isdotdot = vstring_equals ( item , ".." )
       isdot = vstring_equals ( item , "." )
       if ( isdotdot ) then
          ! If the item is a "..", then remember that there is a path to skip and skip the ".." itself
          skipnb = skipnb + 1
          keepitem ( itemindex ) = .false.
       elseif ( isdot ) then
          keepitem ( itemindex ) = .false.
       elseif ( skipnb > 0 ) then
          ! If the current item is a regular path, but there is an item to skip
          ! (because of a previous ".."), skip it
          skipnb = skipnb - 1
          keepitem ( itemindex ) = .false.
       endif
       call vstring_free ( item )
    enddo
    !
    ! 4. Now compute the normalized path by keeping only the required
    ! path components and insert separator between components, if necessary
    !
    call vstring_new ( vfile_normalize )
    separator = vfile_canonicalseparator ( )
    pathindex = 0
    do itemindex = 1 , nbitems
       if ( keepitem ( itemindex ) ) then
          pathindex = pathindex + 1
          item = vstrlist_index ( listOfFiles , itemindex )
          if ( pathindex > 1 ) then
             call vstring_append ( vfile_normalize , separator )
          endif
          call vstring_append ( vfile_normalize , item )
          call vstring_free ( item )
       endif
    enddo
    call vstring_free ( separator )
    !
    ! 5. If the given file is a directory, the name must end with a "/"
    !
    isdirectory = vfile_isdirectory ( vfile_normalize )
    if ( isdirectory ) then
       separator = vfile_canonicalseparator ()
       call vstring_append ( vfile_normalize , separator )
       call vstring_free ( separator )
    endif
    !
    ! 6. Clean-up
    !
    deallocate ( keepitem )
    call vstrlist_free ( listOfFiles )
    
  end function vfile_normalize_vstring
  !
  ! vfile_normalize_charstring --
  !   Returns a unique normalized path representation for the
  !   file-system object (file, directory, link, etc), whose string
  !   value can be used as a unique identifier for it. A normalized path
  !   is an absolute path which has all '../', './' removed. Also it is one which
  !   is in the ``standard'' format for the native platform.
  ! Arguments:
  !   normalized  The normalized file name, with type character(len=*)
  !
#define _TEMPLATE_ROUTINE_NAME_CHARSTRING vfile_normalize_charstring
#define _TEMPLATE_ROUTINE_NAME_VSTRING vfile_normalize_vstring
#define _TEMPLATE_ROUTINE_VALUE type(t_vstring)
#include "m_vfile_template_casttovstring1.f90"
#undef _TEMPLATE_ROUTINE_NAME_CHARSTRING
#undef _TEMPLATE_ROUTINE_NAME_VSTRING
#undef _TEMPLATE_ROUTINE_VALUE
  !
  ! vfile_findByPattern_vstring --
  !   Returns a list of files which match the given pattern
  !
  ! Arguments:
  !   pattern         Pattern for the file names (like: *.f90), with type t_vstring
  !
  ! TODO : extend that subroutine so that it can do an optionnal recursive search into the subdirectories.
  !
  function vfile_findByPattern_vstring ( pattern ) result ( listOfFiles )
    type ( t_vstring ), intent(in)            :: pattern
    type ( t_vstringlist ) :: listOfFiles
    type ( t_vstring ) :: tmpfile
    ! TODO : add features to read from a vstring so that current_filename is made dynamic
    character (len=VFILE_BUFFER_SIZE) :: current_filename
    integer :: luntmp
    integer :: ierr
    integer :: platform
    type ( t_vstring ) :: prefix
    type ( t_vstring ) :: prefix_normalized
    type ( t_vstring ) :: full_filename
    type ( t_vstring ) :: prefix_dirname
    !
    ! 1. Compute the list of files and redirect it to a temporary file.
    !
    tmpfile = vfile_tempfile ()
    !
    ! Execute the ls command
    !
    call vfile_findByPattern_computels ( pattern , tmpfile )
    platform = platform_get_platform ()
    !
    ! Under windows, we only get the file tails.
    ! So we have to normalize these files tails from the pattern 
    ! (which may contain a part of the path, e.g. "dir1/dir2/*.txt") 
    ! to get the full file names.
    !
    if (platform == PLATFORM_PLATFORM_WINDOWS) then
       call vstring_new ( prefix , pattern )
       prefix_normalized = vfile_normalize ( prefix )
       prefix_dirname = vfile_dirname ( prefix_normalized )
    endif
    !
    ! 2. Analyse the content of the temporary file.
    !
    call vfile_findByPattern_opentmpfile ( tmpfile , luntmp )
    !
    ! 2.2 Fill the array
    !
    call vstrlist_new ( listOfFiles )
    do 
       read( luntmp, '(a)' , iostat = ierr ) current_filename
       if ( ierr == 0 ) then
          if (platform == PLATFORM_PLATFORM_WINDOWS) then
             full_filename = vfile_join ( prefix_dirname , trim(current_filename) )
          else
             call vstring_new ( full_filename , trim(current_filename) )
          endif
          call vstrlist_append ( listOfFiles , full_filename )
          call vstring_free ( full_filename )
       else
          exit
       endif
    enddo
    !
    ! 3. Delete the temporary file
    !
    close( luntmp, status = 'delete' )
    !
    ! Clean-up
    !
    if (platform == PLATFORM_PLATFORM_WINDOWS) then
       call vstring_free ( prefix )
       call vstring_free ( prefix_normalized )
       call vstring_free ( prefix_dirname )
    endif
    call vstring_free ( tmpfile )
  contains
    !
    ! vfile_findByPattern_computels --
    !   Execute the "ls" command to get the list of files.
    ! Note :
    !   filename routine may have been inlined directlly in vfile_findByPattern.
    !   But filename internal subroutine allows to use automatic arrays
    !   for the character string tmpfile_charstring.
    !   If, instead, we inline filename subroutine, the character string
    !   has to be declared with a static size.
    !
    subroutine vfile_findByPattern_computels ( pattern , tmpfile )
      type ( t_vstring ), intent(in) :: pattern
      type ( t_vstring ), intent(in) :: tmpfile
      type ( t_vstring ) :: command
      type ( t_vstring ) :: tmpfile_native
      !
      ! Convert the file name to native name to make the platform-specific
      ! command work.
      !
      tmpfile_native = vfile_nativename ( tmpfile )
      call vstring_new ( command )
      call vstring_append ( command , command_ls )
      call vstring_append ( command , " " )
      call vstring_append ( command , pattern )
      call vstring_append ( command , " " )
      call vstring_append ( command , command_redirect )
      call vstring_append ( command , tmpfile_native )
      call vstring_append ( command , " " )
      call vstring_append ( command , command_suppress_msg )
      call vstrplatform_system ( command )
      call vstring_free ( command )
      call vstring_free ( tmpfile_native )
    end subroutine vfile_findByPattern_computels
    !
    ! vfile_findByPattern_opentmpfile --
    !   Open the temporary file containing the ls result
    ! Note :
    !   filename routine may have been inlined directlly in vfile_findByPattern.
    !   But filename internal subroutine allows to use automatic arrays
    !   for the character string tmpfile_charstring.
    !   If, instead, we inline filename subroutine, the character string
    !   has to be declared with a static size.
    !
    subroutine vfile_findByPattern_opentmpfile ( tmpfile , luntmp )
      type ( t_vstring ), intent(in) :: tmpfile
      integer , intent(out) :: luntmp
      luntmp = vfile_open ( tmpfile )
    end subroutine vfile_findByPattern_opentmpfile
  end function vfile_findByPattern_vstring
  !
  ! vfile_findByPattern_charstring --
  !   Returns a list of files which match the given pattern
  !
  ! Arguments:
  !   pattern  Pattern for the file names (like: *.f90), with type character(len=*)
  !
#define _TEMPLATE_ROUTINE_NAME_CHARSTRING vfile_findByPattern_charstring
#define _TEMPLATE_ROUTINE_NAME_VSTRING vfile_findByPattern_vstring
#define _TEMPLATE_ROUTINE_VALUE type(t_vstringlist)
#include "m_vfile_template_casttovstring1.f90"
#undef _TEMPLATE_ROUTINE_NAME_CHARSTRING
#undef _TEMPLATE_ROUTINE_NAME_VSTRING
#undef _TEMPLATE_ROUTINE_VALUE
  !
  ! vfile_split_vstring --
  !   Computes an array whose elements are the path components in name.
  ! Arguments:
  !   listOfComponents   The list of strings, with type t_vstring
  ! TODO : Path separators will be discarded unless they are needed ensure that an element is unambiguously relative.
  !
  function vfile_split_vstring ( filename ) result ( listOfComponents )
    type ( t_vstring ), intent(in) :: filename
    type ( t_vstringlist ) :: listOfComponents
    type ( t_vstring ) :: separator
    type ( t_vstring ) :: canonicalseparator
    separator = vfile_separator ()
    canonicalseparator = vfile_canonicalseparator ( )
    ! Add the canonical separator
    call vstring_append ( separator , canonicalseparator )
    ! vstrlist_split is really powerful
    listOfComponents = vstrlist_split ( filename , separator )
    call vstring_free ( separator )
    call vstring_free ( canonicalseparator )
  end function vfile_split_vstring
  !
  ! vfile_split_charstring --
  !   Computes an array whose elements are the path components in name.
  ! Arguments:
  !   listOfComponents   The list of strings, with type character(len=*)
  !
  function vfile_split_charstring ( filename ) result ( listOfComponents )
    character(len=*), intent(in) :: filename
    type ( t_vstringlist ) :: listOfComponents
    type ( t_vstring ) :: filename_vstring
    call vstring_new ( filename_vstring , filename )
    listOfComponents = vfile_split_vstring ( filename_vstring )
    call vstring_free ( filename_vstring )
  end function vfile_split_charstring
  !
  ! vfile_touch --
  !   Implementation of touch. Alter the atime and mtime of the specified files. 
  ! Arguments:
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !     upon return
  !
  subroutine vfile_touch ( filename , status )
    type ( t_vstring ), intent(in) :: filename
    integer, intent(out) , optional :: status
    integer :: local_status
    integer :: platform
    type ( t_vstring ) :: message
    type ( t_vstring ) :: platform_string

    platform = platform_get_platform ()
    select case ( platform )
    case ( PLATFORM_PLATFORM_WINDOWS )
       call vfile_touch_windows ( filename , local_status )
    case ( PLATFORM_PLATFORM_UNIX )
       call vfile_touch_linux ( filename , local_status )
    case default
       call vstring_new ( message )
       call vstring_append ( message , "Unknown touch command for platform :" )
       platform_string = vstring_format ( platform )
       call vstring_append ( message , platform_string )
       call vstring_free ( platform_string )
       call vstring_append ( message , " in vfile_touch" )
       call vfile_error ( message )
       call vstring_free ( message )
    end select
    if ( present ( status )) then
       status = local_status
    elseif ( local_status /=0 ) then
       call vstring_new ( message )
       call vstring_append ( message , "Unable to touch file :" )
       call vstring_append ( message , filename )
       call vstring_append ( message , " in vfile_touch" )
       call vfile_error ( message )
       call vstring_free ( message )
    endif
  contains
    !
    ! vfile_touch_linux --
    !   Implementation of touch for windows.
    !   A rough hack, based on a copy, to make something available under windows.
    ! TODO : make it modify the modification time.
    !
    subroutine vfile_touch_linux ( filename , local_status )
      type ( t_vstring ), intent(in) :: filename
      integer, intent(out) :: local_status
      type ( t_vstring ) :: command
      type ( t_vstring ) :: filename_native
      !
      ! Convert the file name to native name to make the platform-specific 
      ! command work.
      !
      filename_native = vfile_nativename ( filename )
      call vstring_new ( command )
      call vstring_append ( command , command_touch )
      call vstring_append ( command , " " )
      call vstring_append ( command , filename_native )
      call vstrplatform_system ( command , local_status )
      call vstring_free ( command )
      call vstring_free ( filename_native )
    end subroutine vfile_touch_linux
    !
    ! vfile_touch_windows --
    !   Implementation of touch for windows.
    !   A rough hack, based on a copy, to make something available under windows.
    ! TODO : make it modify the modification time.
    !
    subroutine vfile_touch_windows ( filename , local_status )
      type ( t_vstring ), intent(in) :: filename
      integer, intent(out) :: local_status
      type ( t_vstring ) :: tempfile
      logical :: fexist
      type ( t_vstring ) :: message
      integer :: file_unit
#ifdef _VFILE_STATIC_BUFFER
      character ( len = VFILE_BUFFER_SIZE ) :: filename_charstring
#else
      character ( len = vstring_length(filename)) :: filename_charstring
#endif
      fexist = vfile_exists ( filename )
      local_status = 0
      if ( fexist ) then
         tempfile = vfile_tempfile (  )
         call vfile_delete ( tempfile )
         ! One cannot use the "light" vfile_rename, because it does not modify atime or mtime
         ! So we have to copy
         call vfile_copy ( filename , tempfile )
         call vfile_delete ( filename )
         call vfile_copy ( tempfile , filename )
         call vfile_delete ( tempfile )
         call vstring_free ( tempfile )
      else
         ! If the file does not exist, create it as an empty file
         file_unit = vfile_open ( filename , status ='unknown', iostat= local_status )
         if ( local_status == 0 ) then
            close ( UNIT = file_unit , IOSTAT= local_status )
         else
            call vstring_new ( message )
            call vstring_append ( message , "Unable to touch file :" )
            call vstring_append ( message , filename )
            call vstring_append ( message , " in vfile_touch" )
            call vfile_error ( message )
            call vstring_free ( message )
            return
         endif
      endif
    end subroutine vfile_touch_windows
  end subroutine vfile_touch
  !
  ! vfile_pathtype_vstring --
  !   Returns one of VFILE_PATHTYPE_ABSOLUTE, VFILE_PATHTYPE_RELATIVE, VFILE_PATHTYPE_VOLUMERELATIVE. 
  !   If name refers to a specific file on a specific volume, the path 
  !   type will be absolute. If name refers to a file relative to the current 
  !   working directory, then the path type will be relative. If name refers to 
  !   a file relative to the current working directory on a specified volume, or to 
  !   a specific file on the current working volume, then the path type is volumerelative.
  ! Examples :
  !   "." is relative on all platforms
  !   ".." is relative on all platforms
  !   "/" is absolute on Linux/Unix
  !   "C:/" is absolute on Windows (if the C:/ exists)
  !   "/" is volumerelative on windows and refers to the current volume (for example C:/)
  !   "toto.txt" is relative on all platforms
  !   "./toto.txt" is relative on all platforms
  !
  function vfile_pathtype_vstring ( filename ) result ( pathtype )
    type ( t_vstring ), intent(in) :: filename
    integer :: pathtype
    integer :: platform
    integer :: firstsep
    type ( t_vstring ) :: firstcomp
    type ( t_vstring ) :: nativename
    logical :: equals_dot
    logical :: equals_doubledot
    type ( t_vstringlist ) :: listofvolumes
    integer :: volumeindex
    type ( t_vstring ) :: separator
    nativename = vfile_nativename ( filename )
    firstsep = vfile_first_separator_index ( nativename )
    platform = platform_get_platform ()
    if (firstsep==0) then
       ! There is no separator
       pathtype = VFILE_PATHTYPE_RELATIVE
    elseif (firstsep==1) then
       ! There is one separator, which is the first character
       if ( platform == PLATFORM_PLATFORM_WINDOWS ) then
          pathtype = VFILE_PATHTYPE_VOLUMERELATIVE
       else
          pathtype = VFILE_PATHTYPE_ABSOLUTE
       endif
    else
       firstcomp = vstring_range ( nativename , 1 , firstsep-1 )
       equals_dot = vstring_equals ( firstcomp , "." )
       equals_doubledot = vstring_equals ( firstcomp , ".." )
       if ( equals_dot .OR. equals_doubledot ) then
          pathtype = VFILE_PATHTYPE_RELATIVE
       else
          !
          ! The first item in the path is neither a ".", nor a ".."
          ! and the first character is not a separator.
          ! On Linux and Mac, this is a relative file.
          ! But on Windows, "toto/titi.txt" is relative and 
          ! "C:/titi.txt" is absolute : we have to make the difference 
          ! between "toto", which is just a regular directory name,
          ! and "C:", which is a file volume.
          !
          if ( platform/=PLATFORM_PLATFORM_WINDOWS ) then
             pathtype = VFILE_PATHTYPE_RELATIVE
          else
             listofvolumes = vfile_volumes ( )
             ! Append a canonical separator to search the volume in the list of volumes
             separator = vfile_canonicalseparator ()
             call vstring_append ( firstcomp , VFILE_SLASH )
             call vstring_free ( separator )
             volumeindex = vstrlist_search ( listofvolumes , firstcomp )
             if ( volumeindex /=0 ) then
                pathtype = VFILE_PATHTYPE_ABSOLUTE
             else
                pathtype = VFILE_PATHTYPE_RELATIVE
             endif
             call vstrlist_free ( listofvolumes )
          endif
       endif
       call vstring_free ( firstcomp )
    endif
    call vstring_free ( nativename )
  end function vfile_pathtype_vstring
  !
  ! vfile_pathtype_charstring --
  !   Returns the path type of a character(len=*) filename.
  !
#define _TEMPLATE_ROUTINE_NAME_CHARSTRING vfile_pathtype_charstring
#define _TEMPLATE_ROUTINE_NAME_VSTRING vfile_pathtype_vstring
#define _TEMPLATE_ROUTINE_VALUE integer
#include "m_vfile_template_casttovstring1.f90"
#undef _TEMPLATE_ROUTINE_NAME_CHARSTRING
#undef _TEMPLATE_ROUTINE_NAME_VSTRING
#undef _TEMPLATE_ROUTINE_VALUE
  !
  ! vfile_nativename_vstring --
  !   Returns the platform-specific name of the file. 
  !   filename is useful if the filename is needed to pass to a platform-specific 
  !   call, such as the execution of a system command under Windows or 
  !   AppleScript on the Macintosh.
  !
  function vfile_nativename_vstring ( filename ) result ( nativename )
    type ( t_vstring ), intent(in) :: filename
    type ( t_vstring ) :: nativename
    type ( t_vstring ) :: separator
    separator = vfile_separator ( )
    nativename = vfile_changesep ( filename , separator )
    call vstring_free ( separator )
  end function vfile_nativename_vstring
  !
  ! vfile_nativename_charstring --
  !   Returns the platform-specific name of a characterl(len=*) file. 
  !
#define _TEMPLATE_ROUTINE_NAME_CHARSTRING vfile_nativename_charstring
#define _TEMPLATE_ROUTINE_NAME_VSTRING vfile_nativename_vstring
#define _TEMPLATE_ROUTINE_VALUE type(t_vstring)
#include "m_vfile_template_casttovstring1.f90"
#undef _TEMPLATE_ROUTINE_NAME_CHARSTRING
#undef _TEMPLATE_ROUTINE_NAME_VSTRING
#undef _TEMPLATE_ROUTINE_VALUE
  !
  ! vfile_mkdir --
  !   
  ! Arguments:
  !   status, optional : if supplied, it contains 0 on success or nonzero error code
  !     upon return
  !
  subroutine vfile_mkdir ( filename , status )
    type ( t_vstring ), intent(in) :: filename
    integer, intent(out) , optional :: status
    integer :: local_status
    type ( t_vstring ) :: command
    type ( t_vstring ) :: message
    logical :: isdir
    type ( t_vstring ) :: filename_native
    isdir = vfile_isdirectory ( filename )
    if (isdir) then
       local_status = VFILE_ERROR_CURRENT_OBJECT_IS_NOT_DIRECTORY
    else
       !
       ! Convert the file name to native name to make the platform-specific
       ! command work.
       !
       filename_native = vfile_nativename ( filename )
       call vstring_new ( command )
       call vstring_append ( command , command_mkdir )
       call vstring_append ( command , " " )
       call vstring_append ( command , filename_native )
       call vstrplatform_system ( command , local_status )
       call vstring_free ( command )
       call vstring_free ( filename_native )
    endif
    !
    ! Process errors
    !
    if ( present ( status )) then
       status = local_status
    elseif ( local_status /=0 ) then
       call vstring_new ( message )
       call vstring_append ( message , "Unable to make directory :" )
       call vstring_append ( message , filename )
       call vstring_append ( message , " in vfile_mkdir" )
       call vfile_error ( message )
       call vstring_free ( message )
    endif
  end subroutine vfile_mkdir
  !
  ! vfile_open --
  !   Open a file and returns the unit associated with the opened file.
  !   The command is based on fortran intrinsic "open".
  !   If the optional argument fileunit is provided, it is used to 
  !   open the file.
  !   If not provided, a file unit is automatically computed 
  !   on the base of currently free logical units.
  ! Note: All the options of the intrinsic "open" are provided
  !   with the same behaviour and default values, with some exceptions.
  !   * The file name is mandatory in vfile_open,
  !   even if the "file=" specifier is not mandatory in fortran "open".
  !   This behaviour allows the fortran to manage a status="scratch" 
  !   specifier, which provides a way to manage for temporary 
  !   files internally. Instead, the m_vfile component provides the 
  !   vfile_tempfile service.
  !   * The "err=" option with an error label as argument is not 
  !   provided. The client code may use the iostat option instead.
  !
  function vfile_open ( filename , fileunit , iostat , status , &
       access , form , recl , blank , position , action , delim , &
       pad ) result ( fileunit_real )
    type ( t_vstring ) , intent(in) :: filename
    integer , intent(in) , optional :: fileunit
    character(len=*) , intent(in) , optional :: action
    character(len=*) , intent(in) , optional :: status
    character(len=*) , intent(in) , optional :: access
    character(len=*) , intent(in) , optional :: form
    character(len=*) , intent(in) , optional :: blank
    character(len=*) , intent(in) , optional :: position
    character(len=*) , intent(in) , optional :: delim
    character(len=*) , intent(in) , optional :: pad
    integer , intent(in) , optional :: recl
    integer , intent(out) , optional :: iostat
#ifdef _VFILE_STATIC_BUFFER
    character ( len = VFILE_BUFFER_SIZE ) :: filename_charstring
#else
    character ( len = vstring_length(filename)) :: filename_charstring
#endif
    integer :: fileunit_real
    integer , parameter :: char_length_max = 20
    character(len=char_length_max) :: action_real
    character(len=char_length_max)  :: status_real
    character(len=char_length_max) :: access_real
    character(len=char_length_max) :: form_real
    character(len=char_length_max) :: blank_real
    character(len=char_length_max) :: position_real
    character(len=char_length_max) :: delim_real
    character(len=char_length_max) :: pad_real
    integer :: iostat_real
    type ( t_vstring ) :: filename_native
    type ( t_vstring ) :: message
    !
    ! Process options
    !
    if ( present ( fileunit ) ) then
       fileunit_real = fileunit
    else
       fileunit_real = fileunit_getfreeunit ( )
    endif
    if ( present ( status ) ) then
       status_real = status
    else
       status_real = "unknown"
    endif
    if ( present ( access ) ) then
       access_real = access
    else
       access_real = "sequential"
    endif
    if ( present ( form ) ) then
       form_real = form
    else
       select case ( access_real )
       case ( "sequential" )
          form_real = "formatted"
       case ( "direct" )
          form_real = "unformatted"
       case default
          call vstring_new ( message , "Unknown access :" )
          call vstring_append ( message , access_real )
          call vstring_append ( message , " in vfile_open" )
          call vfile_error ( message )
          call vstring_free ( message )
          return
       end select
    endif
    if ( present ( blank ) ) then
       blank_real = blank
    else
       blank_real = "null"
    endif
    if ( present ( position ) ) then
       position_real = position
    else
       position_real = "asis"
    endif
    if ( present ( action ) ) then
       action_real = action
    else
       action_real = "readwrite"
    endif
    if ( present ( delim ) ) then
       delim_real = delim
       !
       ! The delim specifier is only valid for formatted files.
       !
       if ( form_real /= "formatted" ) then
          call vstring_new ( message , "The delim specifier is valid only for formatted file in vfile_open." )
          call vfile_error ( message )
          call vstring_free ( message )
          return
       endif
    else
       delim_real = "none"
    endif
    if ( present ( pad ) ) then
       pad_real = pad
    else
       pad_real = "yes"
    endif
    !
    ! Convert the name to native before using intrinsic "open"
    !
    filename_native = vfile_nativename ( filename )
    call vstring_cast ( filename_native , filename_charstring )
    call vstring_free ( filename_native )
    if ( present ( recl ) ) then
       !
       ! The delim option is valid only for formatted files.
       !
       select case ( form_real )
       case ( "formatted" )
          open ( unit = fileunit_real , &
            file = filename_charstring , &
            status = status_real, &
            access = access_real , &
            form = form_real ,&
            recl = recl , &
            blank = blank_real , &
            position = position_real , &
            action = action_real, &
            delim = delim_real , &
            pad = pad_real , &
            iostat = iostat_real )
       case ( "unformatted" )
          open ( unit = fileunit_real , &
            file = filename_charstring , &
            status = status_real, &
            access = access_real , &
            form = form_real ,&
            recl = recl , &
            blank = blank_real , &
            position = position_real , &
            action = action_real, &
            pad = pad_real , &
            iostat = iostat_real )
       case default
          call vstring_new ( message , "Unknown format :" )
          call vstring_append ( message , form_real )
          call vstring_append ( message , " in vfile_open" )
          call vfile_error ( message )
          call vstring_free ( message )
          return
       end select
    else
       !
       ! The delim option is valid only for formatted files.
       !
       select case ( form_real )
       case ( "formatted" )
          open ( unit = fileunit_real , &
            file = filename_charstring , &
            status = status_real, &
            access = access_real , &
            form = form_real ,&
            blank = blank_real , &
            position = position_real , &
            action = action_real, &
            delim = delim_real , &
            pad = pad_real , &
            iostat = iostat_real )
       case ( "unformatted" )
          open ( unit = fileunit_real , &
            file = filename_charstring , &
            status = status_real, &
            access = access_real , &
            form = form_real ,&
            blank = blank_real , &
            position = position_real , &
            action = action_real, &
            pad = pad_real , &
            iostat = iostat_real )
       case default
          call vstring_new ( message , "Unknown format :" )
          call vstring_append ( message , form_real )
          call vstring_append ( message , " in vfile_open" )
          call vfile_error ( message )
          call vstring_free ( message )
          return
       end select
    endif
    if ( present ( iostat ) ) then
       iostat = iostat_real
    elseif ( iostat_real /= 0 ) then
       call vstring_new ( message )
       call vstring_append ( message , "Unable to open file :" )
       call vstring_append ( message , filename )
       call vstring_append ( message , " in vfile_open" )
       call vfile_error ( message )
       call vstring_free ( message )
       return
    endif
  end function vfile_open
  !**************************************************************
  !
  ! Static methods.
  ! filename part of the component is dedicated to methods which
  ! does not take a vfile as an argument.
  !

  !
  ! vfile_error --
  !   Manage an error for the module with a file
  ! Arguments :
  !   origin : the name of the subroutine/function which generated the error.
  !   message : the message to display
  !
  subroutine vfile_error ( filename , message )
    type ( t_vstring ) , intent(in), optional :: filename
    type ( t_vstring ), intent(in), optional :: message
    type ( t_vstring ) :: directory
    type ( t_vstring ) :: msgstring
    write ( * , * ) "Error"
    !
    ! Print the message
    !
    if ( present ( message ) ) then
       call vstring_new ( msgstring , "Message: " )
       call vstring_append ( msgstring , message )
       call print_vstring ( msgstring )
       call vstring_free ( msgstring )
    endif
    !
    ! Print the file
    !
    if ( present ( filename ) ) then
       call vstring_new ( msgstring , "File: " )
       call vstring_append ( msgstring , filename )
       call print_vstring ( msgstring )
       call vstring_free ( msgstring )
    endif
    !
    ! Print current directory
    !
    call vfile_pwd (directory)
    call vstring_new ( msgstring , "Current working directory: " )
    call vstring_append ( msgstring , directory )
    call print_vstring ( msgstring )
    call vstring_free ( msgstring )
    call vstring_free ( directory )
    !
    ! Stop
    !
    call vfile_error_stop ()
  contains
    subroutine print_vstring ( mystring )
      type ( t_vstring ), intent(in) :: mystring
#ifdef _VFILE_STATIC_BUFFER
      character ( len = VFILE_BUFFER_SIZE ) :: mycharstring
#else
      character(len=vstring_length(mystring) ) :: mycharstring
#endif
      call vstring_cast ( mystring , mycharstring )
      write(*,*) trim(mycharstring)
    end subroutine print_vstring
  end subroutine vfile_error
  !
  ! vfile_error_stop --
  !   Stop the execution if possible.
  !
  subroutine vfile_error_stop ( )
    if ( vfile_stoponerror ) then
       stop
    endif
  end subroutine vfile_error_stop
  ! 
  ! vfile_set_stoponerror --
  !   Configure the behaviour of the component whenever an 
  !   error is met.
  !   If stoponerror is true, then the execution stops if an error is encountered.
  !   If stoponerror is false, then the execution continues if an error is encountered.
  !   In both cases, a message is displayed on standard output.
  ! 
  subroutine vfile_set_stoponerror ( stoponerror )
    logical , intent(in) :: stoponerror
    vfile_stoponerror = stoponerror
  end subroutine vfile_set_stoponerror
  !
  ! vfile_tempdir --
  !   Returns the temporary directory for the current platform.
  !   The command returns the path of a directory where the caller can
  !   place temporary files, such as "/tmp" on Unix systems.
  !   The algorithm we use to find the correct directory is as follows:
  !   1. The directory named in the TMPDIR environment variable.
  !   2. The directory named in the TEMP environment variable.
  !   3. The directory named in the TMP environment variable.
  !   4. A platform specific location:
  !     Windows
  !         "C:\TEMP", "C:\TMP", "\TEMP", and "\TMP" are tried in that order.
  !     (classic) Macintosh
  !         The TRASH_FOLDER environment variable is used. filename is most likely not correct.
  !     Unix
  !         The directories "/tmp", "/var/tmp", and "/usr/tmp" are tried in that order.
  !
  function vfile_tempdir ( ) result ( tempdir )
    type ( t_vstring ) :: tempdir
    logical :: fexist
    integer :: platform
    logical :: tmpdir_found
    type ( t_vstring ) :: message
    type ( t_vstring ) :: platform_string
    type ( t_vstring ) :: tempdir_canonical
    tmpdir_found = .false.
    !
    ! 1. Try TMPDIR environment variable
    !
    if (.NOT.tmpdir_found) then
       call find_tmpdir_in_environment ( "TMPDIR" , tempdir , tmpdir_found )
       if ( .NOT.tmpdir_found ) then
          call vstring_free ( tempdir )
       endif
    endif
    !
    ! 2. Try TEMP environment variable
    !
    if (.NOT.tmpdir_found) then
       call find_tmpdir_in_environment ( "TEMP" , tempdir , tmpdir_found )
       if ( .NOT.tmpdir_found ) then
          call vstring_free ( tempdir )
       endif
    endif
    !
    ! 3. Try TMP environment variable
    !
    if (.NOT.tmpdir_found) then
       call find_tmpdir_in_environment ( "TMP" , tempdir , tmpdir_found )
       if ( .NOT.tmpdir_found ) then
          call vstring_free ( tempdir )
       endif
    endif
    !
    ! 4. Try platform-specific temporary directories
    !
    if (.NOT.tmpdir_found) then
       platform = platform_get_platform ()
       select case ( platform )
       case ( PLATFORM_PLATFORM_WINDOWS )
          if (.NOT.tmpdir_found) then
             call vstring_new ( tempdir , "C:\TEMP" )
             tmpdir_found = vfile_exists ( tempdir )
             if ( .NOT.tmpdir_found ) then
                call vstring_free ( tempdir )
             endif
          endif
          if (.NOT.tmpdir_found) then
             call vstring_new ( tempdir , "C:\TMP" )
             tmpdir_found = vfile_exists ( tempdir )
             if ( .NOT.tmpdir_found ) then
                call vstring_free ( tempdir )
             endif
          endif
          if (.NOT.tmpdir_found) then
             call vstring_new ( tempdir , "\TEMP" )
             tmpdir_found = vfile_exists ( tempdir )
             if ( .NOT.tmpdir_found ) then
                call vstring_free ( tempdir )
             endif
          endif
          if (.NOT.tmpdir_found) then
             call vstring_new ( tempdir , "\TMP" )
             tmpdir_found = vfile_exists ( tempdir )
             if ( .NOT.tmpdir_found ) then
                call vstring_free ( tempdir )
             endif
          endif
       case ( PLATFORM_PLATFORM_UNIX )
          if (.NOT.tmpdir_found) then
             call vstring_new ( tempdir , "/tmp" )
             tmpdir_found = vfile_exists ( tempdir )
             if ( .NOT.tmpdir_found ) then
                call vstring_free ( tempdir )
             endif
          endif
          if (.NOT.tmpdir_found) then
             call vstring_new ( tempdir , "/var/tmp" )
             tmpdir_found = vfile_exists ( tempdir )
             if ( .NOT.tmpdir_found ) then
                call vstring_free ( tempdir )
             endif
          endif
          if (.NOT.tmpdir_found) then
             call vstring_new ( tempdir , "/usr/tmp" )
             tmpdir_found = vfile_exists ( tempdir )
             if ( .NOT.tmpdir_found ) then
                call vstring_free ( tempdir )
             endif
          endif
       case ( PLATFORM_PLATFORM_MAC )
          if (.NOT.tmpdir_found) then
             call find_tmpdir_in_environment ( "TRASH_FOLDER" , tempdir , tmpdir_found )
             if ( .NOT.tmpdir_found ) then
                call vstring_free ( tempdir )
             endif
          endif
       case default
          call vstring_new ( message )
          call vstring_append ( message , "Unknown temporary directory for platform :" )
          platform_string = vstring_format ( platform )
          call vstring_append ( message , platform_string )
          call vstring_free ( platform_string )
          call vstring_append ( message , " in vfile_tempdir" )
          call vfile_error ( message = message )
          call vstring_free ( message )
       end select
    endif
    !
    ! Generate a message if no environment variable match
    !
    if (.NOT.tmpdir_found) then
       call vstring_new ( message )
       call vstring_append ( message , "Unable to find a temporary directory for platform :" )
          platform_string = vstring_format ( platform )
          call vstring_append ( message , platform_string )
          call vstring_free ( platform_string )
       call vstring_append ( message , " in vfile_tempdir" )
       call vfile_error ( message = message )
       call vstring_free ( message )
    endif
    !
    ! If the temporary directory has been found, update the 
    ! path to make it canonical.
    !
    tempdir_canonical = vfile_changesep ( tempdir )
    call vstring_free ( tempdir )
    call vstring_new ( tempdir , tempdir_canonical )
    call vstring_free ( tempdir_canonical )
  contains
    !
    ! See if the given envvar character string is a environment 
    ! variable and returns the value into tmpdir if the environment
    ! variable exists.
    ! If the environment variable does not exist, the tmpdir 
    ! vstring is set to empty.
    !
    subroutine find_tmpdir_in_environment ( envvar , tmpdir , tmpdir_found )
      character(len=*), intent(in) :: envvar
      type ( t_vstring ) , intent(out) :: tmpdir
      logical , intent(out) :: tmpdir_found
      type ( t_vstring ) :: envvar_value
      integer :: varlentgth
      tmpdir_found = .false.
      envvar_value = vstrplatform_get_environment_variable ( envvar )
      varlentgth = vstring_length(envvar_value)
      call vstring_new ( tmpdir )
      if ( varlentgth/=0 ) then
         call vstring_append ( tmpdir , envvar_value )
         fexist = vfile_exists ( tmpdir )
         if ( fexist ) then
            tmpdir_found = .true.
         endif
      else
      endif
      call vstring_free ( envvar_value )
    end subroutine find_tmpdir_in_environment
  end function vfile_tempdir
  !
  ! vfile_tempfile --
  !   The command generates a temporary file name suitable for writing to, and the 
  !   associated file. The file name will be unique, and the file will be writable and 
  !   contained in the appropriate system specific temp directory. The name of the file 
  !   will be returned as the result of the command.
  ! Arguments:
  !   tempfile   Name of the temporary file
  !
  function vfile_tempfile ( ) result ( tempfile )
    type ( t_vstring ) :: tempfile
    type ( t_vstring ) :: tempdir
    ! Number of characters for the random file tail
    integer , parameter ::  nrand_chars = 10
    type ( t_vstring ) :: randomFileTail
    ! The characters from which the name of the file is made of
    integer , parameter :: setsize = 36
    character, dimension(1:setsize), parameter :: characterSet = (/"a", "b", "c", "d", "e", "f", "g", "h", "i", &
     "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", &
     "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"/)
    integer :: icharacter
    ! Maximum number of tries
    integer , parameter :: maxtries = 10
    integer :: itry
    integer :: random_integer
    real :: alea
    logical :: fexist
    logical :: tempfile_done
    type ( t_vstring ) :: message
    tempdir = vfile_tempdir (  )
    !
    ! 1. Loop over the tries
    !
    tempfile_done = .false.
    call random_seed()
    do itry = 1, maxtries
       !
       ! 1.1 Computes a random file tail
       !
       call vstring_new ( randomFileTail )
       do icharacter = 1, nrand_chars
          call random_number ( alea )
          random_integer = nint( alea * setsize + 1 - alea )
          call vstring_append ( randomFileTail , characterSet ( random_integer ) )
       end do
       !
       ! 1.2 Computes a full file
       !
       tempfile = vfile_join ( tempdir , randomFileTail )
       call vstring_free ( randomFileTail )
       !
       ! 1.3 See if the file allready exists
       !
       fexist = vfile_exists ( tempfile )
       if ( fexist ) then
          !
          ! The current file exists, try another one
          !
          call vstring_free ( tempfile )
       else
          ! If the file does not exist, we have found our temporary file name
          tempfile_done = .true.
          exit
       endif
    end do
    if (.NOT.tempfile_done) then
       call vstring_new ( message )
       call vstring_append ( message , "Unable to create a temporary file in directory :" )
       call vstring_append ( message , tempdir )
       call vstring_append ( message , " in vfile_tempfile_name" )
       call vfile_error ( message )
       call vstring_free ( message )
    else
       call vfile_touch ( tempfile )
    endif
    !
    ! Clean-up
    !
    call vstring_free ( tempdir )
  end function vfile_tempfile
  !
  ! vfile_volumes --
  !   Returns the absolute paths to the volumes mounted on the 
  !   system, as a proper string list. 
  !   On UNIX, the command will always return "/", since all filesystems are 
  !   locally mounted. 
  !   On Windows, it will return a list of the available 
  !   local drives (e.g. {a:/ c:/}).
  !   TODO : On the Macintosh, this will be a list of the mounted drives, both local and network.
  ! Note:
  !   With Intel Fortran, the portability routines provide the "GETDRIVESQQ" function,
  !   which returns the list of current drive as a 26 letters string.
  !
  function vfile_volumes ( ) result ( listofvolumes )
    type ( t_vstringlist ) :: listofvolumes
    type ( t_vstring ) :: message
    integer :: platform
    platform = platform_get_platform ()
    select case ( platform )
    case ( PLATFORM_PLATFORM_WINDOWS )
       listofvolumes = vfile_volumes_windows ()
    case ( PLATFORM_PLATFORM_UNIX )
       listofvolumes = vfile_volumes_unix ()
    case ( PLATFORM_PLATFORM_MAC )
       listofvolumes = vfile_volumes_mac ()
    case default
       call vstring_new ( message )
       call vstring_append ( message , "Unable to compute list of volumes in vfile_volumes" )
       call vfile_error ( message )
       call vstring_free ( message )
       return
    end select
  contains
    !
    ! Specific implementation for Mac
    !
    function vfile_volumes_mac ( ) result ( listofvolumes )
      type ( t_vstringlist ) :: listofvolumes
      type ( t_vstring ) :: message
      call vstrlist_new ( listofvolumes )
      call vstring_new ( message )
      call vstring_append ( message , "Unable to compute list of volumes in vfile_volumes" )
      call vfile_error ( message )
      call vstring_free ( message )
    end function vfile_volumes_mac
    !
    ! Specific implementation for Windows.
    ! If Intel Fortran Portability Routines are provided, use it.
    !
    function vfile_volumes_windows ( ) result ( listofvolumes )
      type ( t_vstringlist ) :: listofvolumes
      type ( t_vstring ) :: message
      integer :: charindex
      type ( t_vstring ) :: letter
      type ( t_vstring ) :: filevolume
      logical :: direxists
#ifdef _VFILE_INTEL_FORTRAN_PORTABILITY_ROUTINES
      character(len=26) :: drives
      integer :: iletter
      logical :: isempty
#endif
      call vstrlist_new ( listofvolumes )
#ifdef _VFILE_INTEL_FORTRAN_PORTABILITY_ROUTINES
      ! On Windows, a sample result of GETDRIVESQQ is drives="A CDE       M       U   YZ"
      drives = GETDRIVESQQ ()
      do iletter = 1 , 26
         call vstring_new ( letter , trim(drives(iletter:iletter)) )
         isempty = vstring_equals ( letter , "" )
         if ( .NOT. isempty ) then
            call vstring_new ( filevolume , letter )
            call vstring_append ( filevolume , ":" )
            call vstring_append ( filevolume , VFILE_SLASH )
            call vstrlist_append ( listofvolumes , filevolume )
            call vstring_free ( filevolume )
         endif
         call vstring_free ( letter )
      enddo
#else
      ! From letter A to letter Z
      do charindex = 65 , 90
         letter = vstring_achar ( charindex )
         call vstring_new ( filevolume , letter )
         call vstring_free ( letter )
         call vstring_append ( filevolume , ":" )
         call vstring_append ( filevolume , VFILE_SLASH )
         direxists = vfile_isdirectory ( filevolume )
         if ( direxists ) then
            call vstrlist_append ( listofvolumes , filevolume )
         endif
         call vstring_free ( filevolume )
      enddo
#endif
    end function vfile_volumes_windows
    !
    ! Specific implementation for Unix
    !
    function vfile_volumes_unix ( ) result ( listofvolumes )
      type ( t_vstringlist ) :: listofvolumes
      type ( t_vstring ) :: message
      call vstrlist_new ( listofvolumes )
      call vstrlist_append ( listofvolumes , VFILE_PLATFORM_SEPARATOR_UNIX )
    end function vfile_volumes_unix
  end function vfile_volumes
end module m_vfile

