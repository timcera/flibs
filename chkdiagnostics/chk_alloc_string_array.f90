! chk_alloc_string_array.f90 --
!     Check the way an array of allocatable strings is expanded
!     Do the strings automatically get a larger length to accommodate it all?
!
program chk_alloc_string_array
    implicit none

    integer                                     :: i
    character(len=:), allocatable, dimension(:) :: array

    write(*,*) 'This program uses an array constructor containing strigns of different lengths'
    write(*,*) 'It may lead to run-time errors'

    !
    ! Initial allocation
    !
    array = ["A", "B"]

    !
    ! Add a longer string at the end
    !
    !array = [character(len=5):: array, "CCCCC"]
    array = [array, "CCCCC"]

    write(*,'(10(3a))') ('>', array(i), '< ', i = 1,size(array) )
    write(*,'(a,i0)') 'String length: ', len(array(1))

    !
    ! Now a longer string at the start
    !

    array = ["A", "B"]

    !array = [character(len=5):: "CCCCC", array]
    array = ["CCCCC", array]

    write(*,'(10(3a))') ('>', array(i), '< ', i = 1,size(array) )
    write(*,'(a,i0)') 'String length: ', len(array(1))

end program chk_alloc_string_array
