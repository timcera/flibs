! test_token.f90 --
!    Incomplete test program for the tokenize module
!
program test_token
   use tokenize
   implicit none

   type(tokenizer)   :: token
   character(len=80) :: string
   character(len=80) :: substring
   integer           :: length

   call set_tokenizer( token, token_whitespace, token_empty, token_empty )
   string = '  tokenizer(     token, whitespace, empty, empty )'
   substring = first_token( token, string, length )
   do while ( length .ge. 0 )
      write(*,*) length, '>>', substring(1:length), '<<'
      substring = next_token( token, string, length )
   enddo

   call set_tokenizer( token,  token_empty, token_csv, token_empty )
   string = 'tokenizer(     token,, whitespace, empty, empty )'
   substring = first_token( token, string, length )
   do while ( length .ge. 0 )
      write(*,*) length, '>>', substring(1:length), '<<'
      substring = next_token( token, string, length )
   enddo

   stop
end program
