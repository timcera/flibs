! test_logging.f90 --
!     Small test program for the report module
!
!     $Id$
!
program test_logging
     use m_logger

     call log_init( 'test_logging.out', .true. )

     call log_msg( 'First message' )
     call log_msg( 'Second message' )

     call log_shutdown

     !
     ! Messages will be appended
     !
     call log_init( 'test_logging.out', .false. )

     call log_msg( 'Second part: no timestamp' )
     call log_msg( 'First message' )
     call log_msg( 'Second message' )

end program test_logging
