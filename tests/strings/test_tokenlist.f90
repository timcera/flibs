! test_tokenlist.f90 --
!    Test program for the tokenlist module
!
!    $Id$
!
program test_tokenlist
   use tokenlists
   implicit none

   type(tokenlist)   :: list
   character(len=80) :: string
   integer           :: i

   !
   ! Plain tokenizer: whitespace
   !
   call list%set_tokenizer( token_whitespace, token_empty, token_empty )
   string = '  tokenizer(     token, whitespace, empty, empty )'

   call list%tokenize( string )

   write(*,*) 'Split on spaces: '
   write(*,*) '>>', trim(string), '<<'
   write(*,*) 'Number of tokens: ', list%number()
   do i = 1,list%number()
      write(*,*) list%length(i), '>>', list%token(i), '<<'
   enddo

   !
   ! Include delimited strings (note the lonely comma after the
   ! delimited substring)
   !
   call list%set_tokenizer( token_whitespace, token_empty, '"' )
   string = ' Just say "Hello, world!", as an example'

   call list%tokenize( string )

   write(*,*) 'Split on spaces, delimiters'
   write(*,*) '>>', trim(string), '<<'
   write(*,*) 'Number of tokens: ', list%number()
   do i = 1,list%number()
      write(*,*) list%length(i), '>>', list%token(i), '<<'
   enddo

end program
