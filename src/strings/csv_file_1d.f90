! cvs_file_1d.f90 --
!     Include file for csv_file.f90:
!     contains the body of the one-dimensional version of the
!     writing routines.
!
    integer, intent(in)                 :: lun
    logical, intent(in), optional       :: advance

    logical                             :: adv
    integer                             :: i

    adv = .true.
    if ( present(advance) ) adv = advance

    do i = 1,size(array)-1
        call csv_write( lun, array(i), .false. )
    enddo
    call csv_write( lun, array(size(array)), adv )
!
! end of body
