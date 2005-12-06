! filedir.f90 --
!     Small module to manipulate file and directory names
!
!     The module contains the following functions:
!     filedir_rootname       Return the name without an extension
!     filedir_extension      Return the extension
!     filedir_basename       Return the name without the directory (if present)
!     filedir_dirname        Return the directory name
!     filedir_concat         Concatenate a directory name and a file name
!     filedir_add_extension  Add an extension to a file name
!
!     The functions actually perform fairly simple string manipulations.
!     It is just that these manipulations occur frequently.
!
module filedir
    implicit none

    character(len=10), parameter, private :: separators = '/\\'
contains

! filedir_rootname --
!     Return the name without the extension (if any)
! Arguments:
!     filename   Name of the file to be examined
! Result:
!     The part of the name _before_ the last "." or the whole name
!     if no "." is present
!
function filedir_rootname( filename ) result (rootname)
    character(len=*)             :: filename

    character(len=len(filename)) :: rootname

    integer                      :: kdot
    integer                      :: kslash

    kdot   = scan( filename, '.', .true. )
    kslash = scan( filename, trim(separators), .true. )

    rootname = filename
    if ( kdot .ne. 0 .and. kdot .gt. kslash+1 ) then
        rootname = filename(1:kdot-1)
    endif
end function filedir_rootname

! filedir_extension --
!     Return the extension (if any)
! Arguments:
!     filename   Name of the file to be examined
! Result:
!     The part of the name _after_ the last "." or empty if none
!     present
!
function filedir_extension( filename ) result (extension)
    character(len=*)             :: filename

    character(len=len(filename)) :: extension

    integer                      :: kdot
    integer                      :: kslash

    kdot   = scan( filename, '.', .true. )
    kslash = scan( filename, trim(separators), .true. )

    extension = ''
    if ( kdot .ne. 0 .and. kdot .gt. kslash+1 ) then
        extension = filename(kdot+1:)
    endif
end function filedir_extension

! filedir_basename --
!     Return the name without the directory (if any)
! Arguments:
!     filename   Name of the file to be examined
! Result:
!     The part of the name _after_ the last directory separator
!
function filedir_basename( filename ) result (basename)
    character(len=*)             :: filename

    character(len=len(filename)) :: basename

    integer                      :: kslash

    kslash = scan( filename, trim(separators), .true. )

    basename = filename
    if ( kslash .gt. 1 ) then
        basename = filename(kslash+1:)
    endif
end function filedir_basename

! filedir_dirname --
!     Return the directory (if any)
! Arguments:
!     filename   Name of the file to be examined
! Result:
!     The part of the name _before_ the last directory separator
!
function filedir_dirname( filename ) result (dirname)
    character(len=*)             :: filename

    character(len=len(filename)) :: dirname

    integer                      :: kslash

    kslash = scan( filename, trim(separators), .true. )

    dirname = filename
    if ( kslash .gt. 1 ) then
        dirname = filename(1:kslash)
    endif
end function filedir_dirname

! filedir_concat --
!     Return the directory plus the file
! Arguments:
!     directory  Name of the directory to be used
!     filename   Name of the file to be used
! Result:
!     Concatenated directory and file names
! Note:
!     We need to establish the separator character!
!     For now: assume / if the directory name has no clue
!
function filedir_concat( directory, filename ) result (fullname)
    character(len=*)             :: directory
    character(len=*)             :: filename

    character(len=len(directory)+len(filename)) :: fullname

    integer                      :: kslash
    character(len=1)             :: dirsep

    kslash = scan( directory, trim(separators), .true. )
    if ( kslash .eq. 0 ) then
        dirsep = separators(1:1)
    elseif ( kslash .eq. len_trim(directory) ) then
        dirsep = ' '
    else
        dirsep = directory(kslash:kslash)
    endif

    fullname = trim(directory) // trim(dirsep) // filename
end function filedir_concat

! filedir_add_extension --
!     Return a new file name with the given extension concatenated
! Arguments:
!     filename   Name of the file to be used
!     extension  Extension to be added
! Result:
!     The file name with an added extension
!
function filedir_add_extension( filename, extension ) result (newname)
    character(len=*)             :: filename
    character(len=*)             :: extension

    character(len=len(filename)+len(extension)+1) :: newname
    integer                      :: kdot

    newname = filename
    kdot    = index( newname, '.', .true. )

    if ( kdot .ne. len_trim(newname) ) then
        kdot = len_trim(newname)+1
        newname(kdot:kdot) = '.'
    endif

    if ( extension(1:1) .ne. '.' ) then
        newname(kdot+1:) = extension
    else
        newname(kdot+1:) = extension(2:)
    endif
end function filedir_add_extension

end module filedir
