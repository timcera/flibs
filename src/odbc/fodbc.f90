! fodbc.90 --
!     Fortran interface to ODBC
!
!     $id$
!
!     TODO:
!     - Almost everything
!     - Get drivers
!
module odbc_types
    implicit none

    integer, parameter         :: dp = kind(1.0d0)

    integer, parameter         :: ODBC_INT    = 1
    integer, parameter         :: ODBC_REAL   = 2
    integer, parameter         :: ODBC_DOUBLE = 3
    integer, parameter         :: ODBC_CHAR   = 4

    character(len=40), parameter :: ODBC_MSACCESS ="MicroSoft Access Driver (*.mdb)"

    type ODBC_STATEMENT
        integer, dimension(2)   :: stmt_handle
    end type ODBC_STATEMENT

    type ODBC_DATABASE
        integer, dimension(2)   :: db_handle   = 0
        integer, dimension(2)   :: stmt_handle = 0 ! For querying table names
        integer                 :: error
        character(len=80)       :: errmsg
    end type ODBC_DATABASE

    type ODBC_COLUMN
        character(len=40)       :: name     = ' '
        character(len=40)       :: type     = ' '
        character(len=40)       :: function = ' '
        integer                 :: type_set
        integer                 :: int_value
        real(kind=dp)           :: double_value
        character(len=80)       :: char_value
    end type ODBC_COLUMN
end module odbc_types

module odbc
    use odbc_types

    implicit none

    private :: typename
    private :: column_func
    private :: stringtof
    private :: stringtoc

    interface odbc_open
        module procedure odbc_open_dsn
        module procedure odbc_open_file
    end interface

!    interface odbc_errmsg
!        module procedure odbc_errmsg_db
!        module procedure odbc_errmsg_stmt
!    end interface

   !
   ! Convenient interfaces
   !
   interface odbc_set_column
      module procedure odbc_set_column_int
      module procedure odbc_set_column_real
      module procedure odbc_set_column_double
      module procedure odbc_set_column_char
   end interface
   interface odbc_get_column
      module procedure odbc_get_column_int
      module procedure odbc_get_column_real
      module procedure odbc_get_column_double
      module procedure odbc_get_column_char
   end interface

contains

! typename --
!    Construct the type and attributes of a column
!    in a new table
! Arguments:
!    column        Column information
!    primary       Name of the primary key
!
character(len=40) function typename( column, primary )
   type(ODBC_COLUMN), intent(in) :: column
   character(len=*), intent(in)    :: primary

   if ( column%name .ne. primary ) then
      typename = column%type
   else
      !write( typename, '(2a)' ) trim(column%type), ' primary key'
      typename = trim(column%type) // ' primary key'
   endif

end function typename


! column_func --
!    Construct the name and function of a column
!    in a new table
! Arguments:
!    column        Column information
!
character(len=80) function column_func( column )
   type(ODBC_COLUMN), intent(in) :: column

   if ( column%function .ne. ' ' ) then
      column_func = trim(column%function) // '(' // trim(column%name) // ')'
   else
      column_func = column%name
   endif

end function column_func


! stringtof --
!    Convert a C string to Fortran
! Arguments:
!    string        String to be converted
!
subroutine stringtof( string )
   character(len=*) :: string

   integer          :: last
   last = index( string, char(0) )
   if ( last .gt. 0 ) then
      string(last:) = ' '
   endif

end subroutine stringtof


! stringtoc --
!    Convert a Fortran string to C
! Arguments:
!    string        String to be converted
! Note:
!    It is assumed that the last character
!    is a space. As this is a private
!    routine, this should have been taken
!    care of in the caller.
!
subroutine stringtoc( string )
   character(len=*) :: string

   integer          :: last

   last = 1 + len_trim(string)
   string(last:last) = char(0)

end subroutine stringtoc


! odbc_column_props --
!    Convenience routine to set the properties of a column
! Arguments:
!    column        Column structure
!    name          Name of the column
!    type          Type of the column
!    length        Length if a string
! Side effects:
!    Fields in column filled
!
subroutine odbc_column_props( column, name, type, length )
   type(ODBC_COLUMN), intent(inout) :: column
   character(len=*), intent(in)       :: name
   integer, intent(in)                :: type
   integer, intent(in), optional      :: length

   integer                            :: length_
   character(len=40)                  :: type_expr

   length_ = 20
   if ( present(length) ) then
      length_ = length
   endif

   column%name     = name
   column%type_set = type

   select case ( type )
   case (ODBC_INT)
      column%type = 'INT'
   case (ODBC_REAL)
      column%type = 'FLOAT'
   case (ODBC_DOUBLE)
      column%type = 'DOUBLE'
   case (ODBC_CHAR)
      write( column%type, '(a,i0,a)' ) 'CHAR(', length_, ')'
   case default
      column%type = 'UNKNOWN!'
   end select

end subroutine odbc_column_props


! odbc_column_query --
!    Convenience routine to query a column or a function of that column
! Arguments:
!    column        Column structure
!    name          Name of the column
!    type          Type of the column
!    length        Length if a string (optional)
!    function      Name of the function to apply (if any)
! Side effects:
!    Fields in column filled
!
subroutine odbc_column_query( column, name, type, length, function )
   type(ODBC_COLUMN), intent(inout)     :: column
   character(len=*), intent(in)           :: name
   integer, intent(in)                    :: type
   integer, intent(in), optional          :: length
   character(len=*), intent(in), optional :: function

   column%function = ' '
   if ( present(function) ) then
      column%function = function
   endif
   if ( present(length) ) then
      call odbc_column_props( column, name, type, length )
   else
      call odbc_column_props( column, name, type )
   endif

end subroutine odbc_column_query


! odbc_set_column_int    --
! odbc_set_column_real   --
! odbc_set_column_double --
! odbc_set_column_char   --
!    Convenience routines to set the value of a column
! Arguments:
!    column        Column structure
!    value         The value to be set
! Side effects:
!    Appropriate value field in column set
!
subroutine odbc_set_column_int( column, value )
   type(ODBC_COLUMN), intent(inout) :: column
   integer, intent(in)                :: value

   column%int_value = value
   column%type_set  = ODBC_INT
end subroutine odbc_set_column_int

subroutine odbc_set_column_real( column, value )
   type(ODBC_COLUMN), intent(inout) :: column
   real, intent(in)                 :: value

   column%double_value = value
   column%type_set  = ODBC_DOUBLE
end subroutine odbc_set_column_real

subroutine odbc_set_column_double( column, value )
   type(ODBC_COLUMN), intent(inout) :: column
   real(kind=dp), intent(in)        :: value

   column%double_value = value
   column%type_set  = ODBC_DOUBLE
end subroutine odbc_set_column_double

subroutine odbc_set_column_char( column, value )
   type(ODBC_COLUMN), intent(inout) :: column
   character(len=*), intent(in)     :: value

   column%char_value = value
   column%type_set  = ODBC_CHAR
end subroutine odbc_set_column_char


! odbc_get_column_int    --
! odbc_get_column_real   --
! odbc_get_column_double --
! odbc_get_column_char   --
!    Convenience routines to get the value of a column
! Arguments:
!    column        Column structure
!    value         Value on return
! Side effects:
!    Value argument will be set
! Note:
!    No attempt is made to convert the value
!    to the requested value. You will have to
!    check this yourself
!
subroutine odbc_get_column_int( column, value )
   type(ODBC_COLUMN), intent(inout) :: column
   integer, intent(out)             :: value

   value = column%int_value
end subroutine odbc_get_column_int

subroutine odbc_get_column_real( column, value )
   type(ODBC_COLUMN), intent(inout) :: column
   real, intent(out)                :: value

   value = column%double_value
end subroutine odbc_get_column_real

subroutine odbc_get_column_double( column, value )
   type(ODBC_COLUMN), intent(inout) :: column
   real(kind=dp), intent(out)       :: value

   value = column%double_value
end subroutine odbc_get_column_double

subroutine odbc_get_column_char( column, value )
   type(ODBC_COLUMN), intent(inout) :: column
   character(len=*), intent(out)    :: value

   value = column%char_value
end subroutine odbc_get_column_char


! odbc_open_dsn --
!     Open the database connection based on the DSN name
!
! Arguments:
!     dsnname       Name of the data source
!     db            Database connection
!
subroutine odbc_open_dsn( dsnname, db )

    character(len=*), intent(in)     :: dsnname
    type(odbc_database), intent(out) :: db

    character(len=len(dsnname)+20)   :: connection_string

    connection_string = "DSN=" // trim(dsnname) // ";"

    call odbc_connect( connection_string, db )
end subroutine odbc_open_dsn


! odbc_open_file --
!     Open the database connection based on the name of a file
!     and the driver that should be used
!
! Arguments:
!     filename      Name of the database file
!     driver        Name of the driver
!     db            Database connection
!
subroutine odbc_open_file( filename, driver, db )

    character(len=*), intent(in)     :: filename
    character(len=*), intent(in)     :: driver
    type(odbc_database), intent(out) :: db

    character(len=len(filename)+len(driver)+100)   :: connection_string

    select case (driver)
        case (odbc_msaccess)
            connection_string = "DBQ=" // trim(filename) // &
                ";DRIVER={" // trim(driver) // "};"
        case default
            db%errmsg = "Unknown driver: " // trim(driver)
            db%error  = -1
            return
    end select

    call odbc_connect( connection_string, db )

end subroutine odbc_open_file


! odbc_connect --
!     Open the database connection based on the connection string
!
! Arguments:
!     connection_string    Full connection string
!     db                   Database connection
!
subroutine odbc_connect( connection_string, db )

    character(len=*), intent(in)            :: connection_string
    type(odbc_database), intent(out)        :: db

    character(len=len(connection_string)+1) :: connectc

    interface
        integer function odbc_connect_c( connectc, db_handle )
            character(len=*)      :: connectc
            integer, dimension(*) :: db_handle
        end function odbc_connect_c
    end interface

    db%db_handle   = 0
    db%error       = 0
    db%errmsg       = ' '

    connectc = connection_string
    call stringtoc( connectc )

    db%error = odbc_connect_c( connectc, db%db_handle )

end subroutine odbc_connect


! odbc_close --
!     Close the database connection
!
! Arguments:
!     db                   Database connection
!
subroutine odbc_close( db )
    type(odbc_database), intent(out)        :: db

    interface
        integer function odbc_close_c( db_handle, stmt_handle )
            integer, dimension(*) :: db_handle
            integer, dimension(*) :: stmt_handle
        end function odbc_close_c
    end interface

    db%error = odbc_close_c( db%db_handle, db%stmt_handle )

end subroutine odbc_close


! odbc_error --
!    Return the last error code
! Arguments:
!    db            Connection to the database
! Returns:
!    Last ODBC error code for this database
!
logical function odbc_error( db )
   type(odbc_database) :: db

   odbc_error = db%error .ne. 0
end function odbc_error


! odbc_errmsg --
!    Print the last error message (TODO)
! Arguments:
!    db            Connection to the database
!
subroutine odbc_errmsg( db )
   type(odbc_database) :: db

    interface
        integer function odbc_get_diagnostics_c( handle, type, idx, state, text )
            integer, dimension(*) :: handle
            integer               :: type
            integer               :: idx
            character(len=*)      :: state
            character(len=*)      :: text
        end function odbc_get_diagnostics_c
    end interface

    integer           :: rc
    integer           :: i
    character(len=10) :: state
    character(len=80) :: text

    do i = 0,10
        rc = odbc_get_diagnostics_c( db%db_handle, 0, i, state, text )

        if ( rc /= 0 ) exit

        call stringtof( state )
        call stringtof( text )

        write(*,*) state, trim(text)
    enddo

end subroutine odbc_errmsg


! odbc_get_data_source --
!     Get the first or the next data source name
!
! Arguments:
!     next                 Get next data source?
!     dsnname              Data source name
!     description          Description of the data source
!     success              Whether we had success or not
!                          (if not, no source was returned)
!
subroutine odbc_get_data_source( next, dsnname, description, success )
    logical, intent(in)                :: next
    character(len=*), intent(out)      :: dsnname
    character(len=*), intent(out)      :: description
    logical, intent(out)               :: success

    interface
        integer function odbc_get_data_source_c( direction, dsnname, description )
            integer               :: direction
            character(len=*)      :: dsnname
            character(len=*)      :: description
        end function odbc_get_data_source_c
    end interface

    integer :: direction
    integer :: error

    direction = merge( 1, 0, next )
    error = odbc_get_data_source_c( direction, dsnname, description )

    if ( error == 0 ) then
        success = .true.
        call stringtof( dsnname )
        call stringtof( description )
    else
        success = .false.
        dsnname = ' '
        description = ' '
    endif

end subroutine odbc_get_data_source


! odbc_get_driver --
!     Get the first or the next installed driver
!
! Arguments:
!     next                 Get next driver
!     driver               Driver name
!     description          Description/attributes of the driver
!     success              Whether we had success or not
!                          (if not, no source was returned)
!
subroutine odbc_get_driver( next, driver, description, success )
    logical, intent(in)                :: next
    character(len=*), intent(out)      :: driver
    character(len=*), intent(out)      :: description
    logical, intent(out)               :: success

    interface
        integer function odbc_get_driver_c( direction, driver, description )
            integer               :: direction
            character(len=*)      :: driver
            character(len=*)      :: description
        end function odbc_get_driver_c
    end interface

    integer :: direction
    integer :: error

    direction = merge( 1, 0, next )
    error = odbc_get_driver_c( direction, driver, description )

    if ( error == 0 ) then
        success = .true.
        call stringtof( driver )
        call stringtof( description )
    else
        success     = .false.
        driver      = ' '
        description = ' '
    endif

end subroutine odbc_get_driver


! odbc_get_table --
!     Get information on the first or the next table
!
! Arguments:
!     db                   Database to query
!     next                 Get next driver
!     table                Table name
!     description          Array of strings describing the table
!                          (the first 5 elements are filled)
!     success              Whether we had success or not
!                          (if not, no source was returned)
!
subroutine odbc_get_table( db, next, table, description, success )
    type(odbc_database), intent(inout)          :: db
    logical, intent(in)                         :: next
    character(len=*), intent(out)               :: table
    character(len=*), dimension(:), intent(out) :: description
    logical, intent(out)                        :: success

    interface
        integer function odbc_get_table_c( db, stmt, direction, &
                nstrings, description )
            integer, dimension(*)          :: db
            integer, dimension(*)          :: stmt
            integer                        :: direction
            integer                        :: nstrings
            character(len=*), dimension(*) :: description
        end function odbc_get_table_c
    end interface

    integer :: direction
    integer :: error
    integer :: nstrings
    integer :: i

    if ( size(description) < 5 ) then
        db%error  = -1
        db%errmsg = 'Too few elements in description array'
        return
    endif

    direction   = merge( 1, 0, next )
    nstrings    = size(description)
    table       = ' '
    description = ' '
    error = odbc_get_table_c( db%db_handle, db%stmt_handle, direction, &
                nstrings, description )

    if ( error == 0 ) then
        success = .true.
        do i = 1,nstrings
            call stringtof( description(i) )
        enddo
        table = description(3)
    else
        success     = .false.
        table       = ' '
        description = ' '
    endif

end subroutine odbc_get_table


! odbc_delete_table --
!    Delete a table
! Arguments:
!    db            Structure for the database
!    tablename     Name of the table to be deleted
! Note:
!    The table can not be recovered, unless this
!    is part of a transaction
!
subroutine odbc_delete_table( db, tablename )
   type(ODBC_DATABASE) :: db
   character(len=*)      :: tablename

   character(len=20+len(tablename)) :: command

   write( command, "(2A)" ) "DELETE TABLE ", tablename
   call odbc_do( db, command )

end subroutine odbc_delete_table


! odbc_create_table --
!    Create a new table
! Arguments:
!    db            Structure for the database
!    tablename     Name of the table
!    columns       Properties of the columns
!    primary       Name of the primary key (if any)
! Side effects:
!    The new table is created
!
subroutine odbc_create_table( db, tablename, columns, primary )
   type(ODBC_DATABASE)              :: db
   character(len=*)                   :: tablename
   type(ODBC_COLUMN), dimension(:)  :: columns
   character(len=*), optional         :: primary

   character(len=20+80*size(columns)) :: command
   character(len=40)                  :: primary_
   integer                            :: i
   integer                            :: ncols

   primary_ = ' '
   if ( present(primary) ) then
      primary_ = primary
   endif

   ncols = size(columns)
   write( command, '(100a)' ) 'create table ', tablename, ' (', &
      ( trim(columns(i)%name), ' ', trim(typename(columns(i), primary_)), ', ', &
           i = 1,ncols-1 ), &
      trim(columns(ncols)%name), ' ', trim(typename(columns(ncols),primary_)), ')'

   call odbc_do( db, command )
end subroutine odbc_create_table

end module


! test_odbc
!     Main program to test the ODBC interface
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

    type(odbc_database)               :: db
    type(odbc_column), dimension(3)   :: column
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
        call odbc_errmsg( db )
    endif

    next    = .false.
    success = .true.

    do while( success )
        call odbc_get_table( db, next, table, table_desc, success )
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

    call odbc_column_props( column(1), 'X', ODBC_CHAR, 10 )
    call odbc_column_props( column(2), 'Y', ODBC_CHAR, 10 )
    call odbc_column_props( column(3), 'Salinity', ODBC_REAL )
    call odbc_column_props( column(4), 'Temperature', ODBC_REAL )

    call odbc_create_table( db, 'measurements', column )

end program test_odbc
