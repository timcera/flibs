! cvs_file_2d.f90 --
!     Include file for csv_file.f90:
!     contains the body of the two-dimensional version of the
!     writing routines.
!
    integer, intent(in)                 :: lun

    logical                             :: adv
    integer                             :: i

    adv = .true.

    do i = 1,size(array,2)
        call csv_write( lun, array(:,i), adv )
    enddo
!
! end of body
