! test_base64.f90 --
!     Test program for the base64 module
!
!     Implementation based on the Wikipedia page.
!     No variants supported.
!
! test --
!     Use the examples from the Wikipedia page:
!
!     Original                  Encoded                           Padding
!     "any carnal pleasure."    "YW55IGNhcm5hbCBwbGVhc3VyZS4="    1
!     "any carnal pleasure"     "YW55IGNhcm5hbCBwbGVhc3VyZQ=="    2
!     "any carnal pleasur"      "YW55IGNhcm5hbCBwbGVhc3Vy"        0
!
program test_base64
    use base64

    implicit none

    character(len=:), allocatable :: encoded_string, output
    logical                       :: error

    call encode( 'any carnal pleasur', encoded_string )
    write(*,*) '>', encoded_string, '<'
    call decode( encoded_string, output, error )
    write(*,*) 'Error? ', error
    if ( .not. error ) then
        write(*,*) '>', output, '<'
    endif

    call encode( 'any carnal pleasure', encoded_string )
    write(*,*) '>', encoded_string, '<'
    call decode( encoded_string, output, error )
    write(*,*) 'Error? ', error
    if ( .not. error ) then
        write(*,*) '>', output, '<'
    endif

    call encode( 'any carnal pleasure.', encoded_string )
    write(*,*) '>', encoded_string, '<'
    call decode( encoded_string, output, error )
    write(*,*) 'Error? ', error
    if ( .not. error ) then
        write(*,*) '>', output, '<'
    endif

    !
    ! Invalid base64 string ...
    write(*,*) 'Trying to decode an invlaid string:'
    call decode( "YW55IGNhcm5hbCBwbGVhc3VyZS.=", output, error )
    write(*,*) 'Error? ', error

end program test_base64
