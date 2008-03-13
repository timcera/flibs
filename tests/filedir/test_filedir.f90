!
! This program is a testing component for the module m_products.
! Functions tested :
!  public :: filedir_add_extension : OK
!  public :: filedir_atime : OK
!  public :: filedir_tail : OK
!  public :: filedir_copy : OK
!  public :: filedir_copy_std : OK
!  public :: filedir_delete : OK
!  public :: filedir_exists : OK
!  public :: filedir_extension : OK
!  public :: filedir_get_cwd : OK
!  public :: filedir_get_unit : OK
!  public :: filedir_join : OK
!  public :: filedir_rename : OK
!  public :: filedir_dirname : TODO
!  public :: filedir_rootname : TODO
!  public :: filedir_normalize : EXTREMERLY PARTIALLY
!  public :: filedir_separator : IMPLICITELY
!
program test_filedir
  use filedir, only : &
       filedir_rename, &
       filedir_copy, &
       filedir_copy_std, &
       filedir_get_unit, &
       filedir_get_cwd, &
       filedir_exists, &
       filedir_delete, &
       filedir_atime, &
       filedir_extension, &
       filedir_tail, &
       filedir_join, &
       filedir_add_extension , &
       filedir_normalize, &
       filedir_findByPattern, &
       filedir_tmpdir, &
       filedir_init, &
       filedir_split
       
  implicit none
  character (len= 100 ) :: cwd
  integer :: file_unit, other_file_unit
  character (len= 200 ), parameter :: source_file = "declaration.txt"
  character (len= 200 ), parameter :: target_file = "declaration_copy.txt"
  character (len= 200 ), parameter :: file_without_extension = "declaration"
  character (len= 200 ), parameter :: file_with_dot = "declaration."
  character (len= 200 ) :: normalized_source_file
  character (len= 200 ) :: result_file_name
  character (len= 200 ) :: computed
  character (len= 200 ) :: expected
  !character(len=50), dimension(:), pointer :: listOfFiles => NULL()
  !character(len=50), dimension(1:50) :: listOfFiles
  character, dimension(:,:), pointer :: listOfFiles => NULL()
  integer                                  :: ifile
  character, dimension(:,:), pointer :: listOfComponents => NULL()
  external :: file_error
  logical :: fexist
  integer :: status
  integer, parameter :: MSG_LEN = 500
  character (len= MSG_LEN ) :: msg
  integer :: atime
  logical :: force = .true.
  character (len=10) :: fext
  character (len=20) :: ftail
  character(len=200)             :: tmpdir
  character(len=20)            :: pattern
  integer, parameter :: filelength = 200
  integer :: numberOfFiles
  integer :: numberOfComponents
  integer :: iComponent
  character(len=20)            :: computedComponent
  integer :: numberOfChars
  !
  call filedir_init ()
  !
  ! test #1 : current working directory
  !
  call logmsg ( "Test #1 : get_cwd" )
  ! Just test that it works
  call filedir_get_cwd ( cwd )
  write ( msg ,*) "Current working directory:"
  call logmsg ( msg )
  write ( msg ,*) trim(cwd)
  call logmsg ( msg )
  !
  ! test #2 : get an unused file unit
  !
  call logmsg ( "Test #2 : get_unit" )
  file_unit = filedir_get_unit (  )
  write ( msg ,*)  "Unused unit:", file_unit
  call logmsg ( msg )
  call assert (file_unit==1, "Wrong file unit. (1)")
  other_file_unit = filedir_get_unit (  )
  call assert (other_file_unit==1, "Wrong file unit. (2)")
  open ( file_unit , file = source_file )
  other_file_unit = filedir_get_unit (  )
  write ( msg ,*)  "Unused unit:", other_file_unit
  call logmsg ( msg )
  call assert (other_file_unit==2, "Wrong file unit. (3)")
  close ( file_unit )
  other_file_unit = filedir_get_unit (  )
  call assert (other_file_unit==1, "Wrong file unit. (4)")
  !
  ! Test #3
  !
  call logmsg ( "Test #3 : copy_std" )
  call filedir_copy_std ( source_file , target_file , status )
  call assert ( status==0 , "Error while copying the file (3)")
  fexist = filedir_exists (target_file)
  call assert ( fexist , "File copy does not exist (3)")
  !
  ! Test #4
  !
  call logmsg ( "Test #4 : copy" )
  call filedir_copy ( source_file , target_file , status )
  fexist = filedir_exists (target_file)
  call assert ( fexist , "File copy does not exist (4)")
  call assert ( status==0 , "Error while copying the file (4)")
  !
  ! Test #5
  ! Prove that one can copy several times the same file with filedir_copy,
  ! therefore overwriting the same file the second time
  !
  call logmsg ( "Test #5 : copy with overwrite" )
  call filedir_copy ( source_file , target_file , status )
  fexist = filedir_exists (target_file)
  call assert ( fexist , "File copy does not exist (5)")
  call assert ( status==0 , "Error while copying the file (5)")
  !
  ! Test #6
  !
  call logmsg ( "Test #6 : delete" )
  call filedir_delete ( target_file )
  fexist = filedir_exists (target_file)
  call assert ( .NOT.fexist , "File copy exists (6)")
  !
  ! Test #7
  !
  call logmsg ( "Test #7 : copy" )
  call filedir_copy ( source_file , target_file , status )
  call filedir_delete ( target_file , status )
  call assert ( status==0 , "Error while deleting file (7).")
  !
  ! Test #8
  !
  call logmsg ( "Test #8 : rename" )
  call filedir_rename ( source_file , target_file , status )
  call assert ( status==0 , "Error while renaming file (8).")
  fexist = filedir_exists ( target_file )
  call assert ( fexist , "File renamed does not exist (8)")
  call filedir_rename ( target_file , source_file )
  !
  ! Test #9
  !
  call logmsg ( "Test #9 : atime" )
  write ( msg ,*)  "File :", source_file
  call logmsg ( msg )
  atime = filedir_atime ( source_file )
  write ( msg ,*)  "Atime :", atime
  call logmsg ( msg )
  call assert ( atime>=0 , "Error while getting acces time (9).")
  !
  ! Test #10
  ! Try to force a copy.
  !
  call logmsg ( "Test #10 : copy -force" )
  call filedir_copy_std ( source_file , target_file )
  call filedir_copy_std ( source_file , target_file , status , force )
  fexist = filedir_exists (target_file)
  call assert ( fexist , "File copy does not exist (10)")
  call assert ( status==0 , "Error while copying the file (10)")
  call filedir_delete ( target_file )
  !
  ! Test #11 : file extension
  !
  call logmsg ( "Test #11 : file extension (1)" )
  write ( msg ,*)  "File :", source_file
  call logmsg ( msg )
  fext = filedir_extension ( source_file )
  write ( msg ,*)  "Extension :", fext
  call logmsg ( msg )
  call assert ( fext(1:4)==".txt" , "Error in file extension.")
  !
  ! Test #12 : file extension, without extension
  !
  call logmsg ( "Test #12 : file extension without extension (2)" )
  write ( msg ,*)  "File :", file_without_extension
  call logmsg ( msg )
  fext = filedir_extension ( file_without_extension )
  write ( msg ,*)  "Extension :", fext
  call logmsg ( msg )
  call assert ( trim(fext)=="" , "Error in file extension (2).")
  !
  ! Test #13 : file tail, without dirname
  !
  call logmsg ( "Test #13 : file tail without dirname (1)" )
  write ( msg ,*)  "File :", source_file
  call logmsg ( msg )
  ftail = filedir_tail ( source_file )
  write ( msg ,*)  "Tail :", ftail
  call logmsg ( msg )
  call assert ( trim(ftail)=="declaration.txt" , "Error in file tail (1).")
  !
  ! Test #14 : file tail, with dirname (test also filedir_join)
  !
  call logmsg ( "Test #14 : file tail with dirname (2)" )
  normalized_source_file = filedir_join ( cwd , source_file )
  write ( msg ,*)  "File :", normalized_source_file
  call logmsg ( msg )
  ftail = filedir_tail ( normalized_source_file )
  write ( msg ,*)  "Tail :", ftail
  call logmsg ( msg )
  call assert ( trim(ftail)=="declaration.txt" , "Error in file tail (1).")
  !
  ! Test #15 : add extension
  !
  call logmsg ( "Test #15 : add extension (1)" )
  write ( msg ,*)  "File :", file_without_extension
  call logmsg ( msg )
  result_file_name = filedir_add_extension ( file_without_extension , ".txt" )
  write ( msg ,*)  "With extension :", result_file_name
  call logmsg ( msg )
  call assert ( trim(source_file)==trim(result_file_name) , "Error in add extension (1).")
  !
  ! Test #16 : add extension to a file with allready one dot at the end
  !
  call logmsg ( "Test #16 : add extension (2)" )
  write ( msg ,*)  "File :", file_with_dot
  call logmsg ( msg )
  result_file_name = filedir_add_extension ( file_with_dot , ".txt" )
  write ( msg ,*)  "With extension :", result_file_name
  call logmsg ( msg )
  call assert ( trim(source_file)==trim(result_file_name) , "Error in add extension (2).")
  !
  ! Test #17 : normalize
  !
  call logmsg ( "Test #17 : normalize" )
  write ( msg ,*)  "File :", source_file
  call logmsg ( msg )
  computed = filedir_normalize ( source_file )
  write ( msg ,*)  "Normalized :", computed
  call logmsg ( msg )
  call filedir_get_cwd ( cwd )
  expected = filedir_join ( cwd , source_file )
  call assert ( trim(expected)==trim(computed) , "Error in normalize.")
  !
  ! Test #18 : get temporary directory
  !
  call logmsg ( "Test #18 : temporary directory" )
  call filedir_tmpdir ( tmpdir )
  write ( msg ,*)  "Temporary directory :", tmpdir
  call logmsg ( msg )
  !
  ! Test #19 : find files by pattern
  !
  call logmsg ( "Test #19 : find files by pattern" )
  pattern = "*.txt"
  call filedir_findByPattern ( pattern , filelength , numberOfFiles , listOfFiles )  
  call logmsg ("Files: ")
  write ( msg ,*)  "Number :", numberOfFiles
  call logmsg ( msg )
  call assert ( numberOfFiles == 2 , "Error in filedir_findByPattern.")
  ! TODO : test that the file tails are the expected ones : declaration.txt and declaration2.txt
  do ifile =1 , numberOfFiles
     write ( msg ,*)  "File #", ifile, ":", listOfFiles ( : , ifile )
     call logmsg (msg)
  enddo
  deallocate( listOfFiles )
  !
  ! Test #20 : split a file name
  !
  call logmsg ( "Test #20 : split a file name" )
  normalized_source_file = filedir_normalize ( source_file )
  call filedir_split ( normalized_source_file , numberOfComponents , numberOfChars , &
       listOfComponents )
  write ( msg ,*)  "Number of components:", numberOfComponents
  call logmsg ( msg )
  write ( msg ,*)  "Number of characters:", numberOfChars
  call logmsg ( msg )
  do iComponent =1 , numberOfComponents
     write ( msg ,*)  "Component #", iComponent , ":", listOfComponents ( 1:numberOfChars , iComponent )
     call logmsg (msg)
     if (iComponent==numberOfComponents) then
        write ( computedComponent , "(15a)" ) listOfComponents ( : , iComponent )
        call assertString ( trim(computedComponent) , "declaration.txt" , "Error in filedir_split")
     endif
  enddo
  deallocate ( listOfComponents )
  !
  ! Shutdown the tests
  !
  call logmsg ( "Test suite completed" )

contains
  subroutine assert (test, message)
    implicit none
    logical         , intent(in) :: test
    character(len=*), intent(in) :: message
    character(len=50) :: origin
    integer, parameter :: MSG_LEN = 200
    character (len= MSG_LEN ) :: msg
    origin = "test_filedir.f90"
    if (.NOT.test) then
       write(msg,*) "Internal error from: ", trim(origin)
       call logmsg ( msg )
       write(msg,*) "Error: ", trim(message)
       call logmsg ( msg )
       STOP
    else
       write(msg,*) "-> Test PASS"
       call logmsg ( msg )       
    endif
  end subroutine assert
  subroutine assertString ( computedString , expectedString , message )
    implicit none
    character(len=*), intent(in) :: computedString
    character(len=*), intent(in) :: expectedString
    character(len=*), intent(in) :: message
    integer, parameter :: MSG_LEN = 200
    integer :: stringlength
    integer :: icharacter
    logical :: test
    character :: computedCharacter
    character :: expectedCharacter
    integer :: computedLength
    integer :: expectedLength
    computedLength = len(computedString)
    expectedLength = len(expectedString)
    call assert ( computedLength==expectedLength , message )
    stringlength = computedLength
    test = .true.
    do icharacter = 1 , stringlength
       computedCharacter (1:)= computedString (icharacter:icharacter)
       expectedCharacter (1:)= expectedString (icharacter:icharacter)
       if (computedCharacter/=expectedCharacter) then
          test = .false.
          exit
       endif
    enddo
    call assert ( test , message )
  end subroutine assertString
  subroutine logmsg ( message )
    implicit none
    character(len=*), intent(in) :: message
    write(6,*) trim(message)
  end subroutine logmsg
end program test_filedir

!
! Display an error and STOP the execution.
!
subroutine file_error ( command )
  use filedir, only : MAX_COMMAND_LENGTH
  implicit none
  character (len= MAX_COMMAND_LENGTH ), intent(in) :: command
  character (len= 40 ) :: cwd
  write (6,*) "Error in operating on files while executing command :"
  write (6,*) trim(command)
  call GETCWD (cwd)
  write (6,*) "Current working directory : ", trim(cwd)
  STOP
end subroutine file_error

