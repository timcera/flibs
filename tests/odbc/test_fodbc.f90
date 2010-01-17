! test_fodbc.f90 --
!     Test program for the ODBC module
!
program test_odbc

    use odbc

    implicit none

    logical                           :: next
    logical                           :: success
    character(len=40)                 :: driver
    character(len=40)                 :: dsnname
    character(len=40)                 :: table
    character(len=200)                :: description
    character(len=200), dimension(10) :: table_desc
    character(len=10)                 :: param
    integer                           :: i
    integer                           :: colno
    logical                           :: finished
    character(len=10)                 :: date
    real(dp)                          :: value1
    real(dp)                          :: value2

    type(odbc_database)               :: db
    type(odbc_statement)              :: stmt
    type(odbc_column), dimension(4), target  :: column
    type(odbc_column), dimension(:), pointer :: p_columns
    !
    ! First, simple step: check what data sources we have
    !

    next    = .false.
    success = .true.

    write(*,'(a,/)') 'Overview of installed drivers:'
    do while( success )
        call odbc_get_driver( next, driver, description, success )
        if ( success ) then
            write(*,'(2a)') driver, trim(description)
        endif

        next = .true.
    enddo

    next    = .false.
    success = .true.

    write(*,'(/,a,/)') 'Overview of data sources:'
    do while( success )
        call odbc_get_data_source( next, dsnname, description, success )
        if ( success ) then
            write(*,'(2a)') dsnname, trim(description)
        endif

        next = .true.
    enddo

    write(*,'(/,a)') 'Opening a particular database file:'
    write(*,'(a)')   'List of tables:'

    call odbc_open( "rand.mdb", odbc_msaccess, db )
!   call odbc_open( "randdb", db )
    if ( odbc_error(db) ) then
        call odbc_errmsg_print( db )
    endif

    next    = .false.
    success = .true.

    do while( success )
        call odbc_get_table_name( db, next, table, table_desc, success )
        if ( success ) then
            write(*,'(2a)') table, trim(table_desc(4))
        endif

        next = .true.
    enddo

    call odbc_close( db )

    !
    ! Create a new table
    !
    call odbc_open( "measurements.mdb", odbc_msaccess, db )
    if ( odbc_error(db) ) then
        call odbc_errmsg_print( db )
    endif

    call odbc_column_props( column(1), 'X', ODBC_CHAR, 10 )
    call odbc_column_props( column(2), 'Y', ODBC_CHAR, 10 )
    call odbc_column_props( column(3), 'Salinity', ODBC_REAL )
    call odbc_column_props( column(4), 'Temperature', ODBC_REAL )

    call odbc_create_table( db, 'measurements', column )

    !
    ! Insert a few rows -- this does not work yet!
    !
    if ( .true. ) then
    call odbc_set_column( column(1), 'A' )
    call odbc_set_column( column(2), 'B' )
    call odbc_set_column( column(3), 10.0 )
    call odbc_set_column( column(4), 12.0 )
    call odbc_insert( db, 'measurements', column )
    if ( odbc_error(db) ) then
        call odbc_errmsg_print( db )
    endif

    call odbc_set_column( column(1), 'C' )
    call odbc_set_column( column(2), 'D' )
    call odbc_set_column( column(3), 20.0 )
    call odbc_set_column( column(4), 3.3 )
    call odbc_insert( db, 'measurements', column )
    endif

    call odbc_column_props( column(1), 'kolomnummer', ODBC_INT )
    call odbc_column_props( column(2), 'parameter', ODBC_CHAR, 10 )

    p_columns => column(1:2)
    call odbc_prepare_select( db, 'KolomBetekenis', p_columns, stmt )

    finished = .false.
    do while ( .not. finished )
        call odbc_next_row( stmt, p_columns, finished )

        if ( .not. finished ) then
            call odbc_get_column( p_columns(1), colno )
            call odbc_get_column( p_columns(2), param )

            write(*,*) colno, param
        endif
    enddo
    call odbc_close( db )

    write(*,*) 'Data:'

    call odbc_open( "example.mdb", odbc_msaccess, db )
    if ( odbc_error(db) ) then
        call odbc_errmsg_print( db )
    endif

    write(*,*) 'Columns of table "example"'
    p_columns => null()
    call odbc_query_table( db, 'example', p_columns )
    do i = 1,size(p_columns)
        write(*,'(1x,a20,a,a20)') p_columns(i)%name, ' -- ', p_columns(i)%type
    enddo

    deallocate( p_columns )

    call odbc_column_props( column(1), 'date', ODBC_CHAR, 10 )
    call odbc_column_props( column(2), 'value', ODBC_DOUBLE )
    call odbc_column_props( column(3), 'another', ODBC_DOUBLE )

    p_columns => column(1:3)
    call odbc_prepare_select( db, 'example', p_columns, stmt )

    finished = .false.
    do while ( .not. finished )
        call odbc_next_row( stmt, p_columns, finished )

        if ( .not. finished ) then
            call odbc_get_column( p_columns(1), date )
            call odbc_get_column( p_columns(2), value1 )
            call odbc_get_column( p_columns(3), value2 )

            write(*,*) date, value1, value2
        endif
    enddo

    call odbc_finalize( stmt )
    call odbc_close( db )

    !
    ! Getting information from an MS Excel file
    !

    write(*,'(a)')   'Example with MS Excel:'
    write(*,'(a)')   'List of tables:'

    call odbc_open( "example.xls", odbc_msexcel, db )
    if ( odbc_error(db) ) then
        call odbc_errmsg_print( db )
    endif

    next    = .false.
    success = .true.

    do while( success )
        call odbc_get_table_name( db, next, table, table_desc, success )
        if ( success ) then
            write(*,'(2a)') table, trim(table_desc(4))
        endif

        next = .true.
    enddo

    call odbc_close( db )
end program test_odbc
