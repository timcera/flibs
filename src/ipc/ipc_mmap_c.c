/* ipc_mmap_c.c --
       Auxiliary routines for dealing with memory-mapped files

       Note:
       This has been tested to work on native Windows, Cygwin
       and Linux (the latter two if you define the LINUX macro)
       (MingW does not compile this code - sys/mman.h is missing)

       TODO:
       A lot!
*/

#ifndef LINUX
#include <windows.h>

#define FTNCALL __stdcall

#else
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/fcntl.h>
#define caddr_t void *

typedef int HANDLE;

#define FTNCALL

#endif

#include <stdlib.h>
#include <stdio.h>

/* Typedefs and globals
*/
typedef struct _CommStruct {
    int    *data;      /* Pointer to the actual data */
    HANDLE  hfile;     /* Handle to the mmapped file */
    int     maxsize;
    char    src[21];
    char    dest[21];
} CommStruct;

static CommStruct *comm = NULL;
static int noConnects = 0;

/* fsleep --
       Sleep for a while - Fortran interface

   Arguments:
       wait         Number of ms to wait
*/
void FTNCALL fsleep(
    int *wait
    ) {

#ifdef LINUX
    usleep( (*wait) * 1000 );
#else
    Sleep( (*wait) );
#endif
}

/* ipc_start_c --
       Find or prepare a large enough mmapped file

   Arguments:
       src          String identifying source
       dest         String identifying destination
       maxsize      Maximum size of individual message
       id           (Returned) ID for the mmapped file
*/
void FTNCALL ipc_start_c(
    char *src,
#ifdef INBETWEEN
    int len_src,
#endif
    char *dest,
#ifdef INBETWEEN
    int len_dest,
#endif
    int *maxsize,
    int *id
#ifndef INBETWEEN
   ,int len_src
   ,int len_dest
#endif
    ) {

#ifndef LINUX
    HANDLE hfile;
    HANDLE hmap;
#else
    int    fd;
#endif

    char   filename[256];
    int   *pdata;
    int    found;

    /* TODO: search in the list of mmapped files for an existing one
    */
    found = 0;

    /* SEARCH */

    if ( !found ) {
        *id = 0;
        noConnects ++;
        if ( comm == NULL ) {
            comm = (CommStruct *) malloc( noConnects * sizeof(CommStruct) );
        } else {
            comm = (CommStruct *) realloc( comm, noConnects * sizeof(CommStruct) );
        }
        strcpy( comm[*id].src,  src  );
        strcpy( comm[*id].dest, dest );
        comm[*id].maxsize = *maxsize;

        /* Create the mmapped file
           Note: With the Linux style, we need to fill the file with
           enough data.

           TODO: directory
        */
        strcpy( filename, src     );
        strcat( filename, "-"     );
        strcat( filename, dest    );
        strcat( filename, ".mmap" );

#ifndef LINUX
        hfile = CreateFile( filename, GENERIC_WRITE | GENERIC_READ,
                    FILE_SHARE_WRITE, NULL, CREATE_ALWAYS,
                    FILE_ATTRIBUTE_TEMPORARY, NULL );

        hmap = CreateFileMapping( hfile, NULL, PAGE_READWRITE, 0, (*maxsize), "MAP" );

        comm[*id].data  = (int *) MapViewOfFile( hmap, FILE_MAP_ALL_ACCESS, 0, 0, 0 );
        comm[*id].hfile = hfile;
#else
        fd = open( filename, O_CREAT|O_RDWR, S_IRWXU );
        pdata = (int *) malloc( (*maxsize + sizeof(int) - 1) / sizeof(int) );
        pdata[0] = 0;
        write( (size_t)(*maxsize), pdata, fd );
        free( pdata );
        comm[*id].data  = (int *) mmap( (caddr_t)0, (*maxsize),
            PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0 );
        comm[*id].hfile = fd;
#endif
    }
}

/* ipc_set_data_c --
       Set a single integer value

   Arguments:
       idcomm       Communication ID
       pos          Position of the integer
       value        New value
*/
void FTNCALL ipc_set_data_c( int *idcomm, int *pos, int *value ) {

    comm[*idcomm].data[*pos] = *value ;
}

/* ipc_get_data_c --
       Get a single integer value

   Arguments:
       idcomm       Communication ID
       pos          Position of the integer
       value        Current value of the integer
*/
void FTNCALL ipc_get_data_c( int *idcomm, int *pos, int *value ) {

    *value = comm[*idcomm].data[*pos] ;
}

/* ipc_send_int_c --
       Send an array of integers

   Arguments:
       idcomm       Communication ID
       pos          Position of the integer
       data         Array with values to send
       number       Number of values to send
*/
void FTNCALL ipc_send_int_c( int *idcomm, int *pos, int *data, int *number ) {
    int  i      ;

    for ( i = 0 ; i < (*number) ; i ++ ) {
        comm[*idcomm].data[i + (*pos)] = data[i] ;
    }
}

/* ipc_send_real_c --
       Send an array of single precision reals

   Arguments:
       idcomm       Communication ID
       pos          Position of the integer
       data         Array with values to send
       number       Number of values to send

   NOTE:
       The type for the "data" argument remains the same!
*/
void FTNCALL ipc_send_real_c( int *idcomm, int *pos, int *data, int *number ) {
    int  i      ;

    for ( i = 0 ; i < (*number) ; i ++ ) {
        comm[*idcomm].data[i + (*pos)] = data[i] ;
    }
}

/* ipc_send_dbl_c --
       Send an array of double precision reals

   Arguments:
       idcomm       Communication ID
       pos          Position of the integer
       data         Array with values to send
       number       Number of values to send

   NOTE:
       The type for the "data" argument remains the same!
*/
void FTNCALL ipc_send_dbl_c( int *idcomm, int *pos, int *data, int *number ) {
    int  i      ;

    for ( i = 0 ; i < (*number) ; i ++ ) {
        comm[*idcomm].data[i + (*pos)] = data[i] ;
    }
}

/* ipc_send_log_c --
       Send an array of logicals

   Arguments:
       idcomm       Communication ID
       pos          Position of the integer
       data         Array with values to send
       number       Number of values to send

   NOTE:
       The type for the "data" argument remains the same!
*/
void FTNCALL ipc_send_log_c( int *idcomm, int *pos, int *data, int *number ) {
    int  i      ;

    for ( i = 0 ; i < (*number) ; i ++ ) {
        comm[*idcomm].data[i + (*pos)] = data[i] ;
    }
}

/* ipc_send_cmplx_c --
       Send an array of single precision complex numbers

   Arguments:
       idcomm       Communication ID
       pos          Position of the integer
       data         Array with values to send
       number       Number of values to send

   NOTE:
       The type for the "data" argument remains the same!
*/
void FTNCALL ipc_send_cmplx_c( int *idcomm, int *pos, int *data, int *number ) {
    int  i      ;

    for ( i = 0 ; i < (*number) ; i ++ ) {
        comm[*idcomm].data[i + (*pos)] = data[i] ;
    }
}

/* ipc_send_char_c --
       Send a character string

   Arguments:
       idcomm       Communication ID
       pos          Position of the integer
       string       String to send
       len_string   Length of the string
*/
void FTNCALL ipc_send_char_c( int *idcomm, int *pos, char *string, int len_string ) {

    memcpy( &comm[*idcomm].data[(*pos)], string, len_string ) ;
}

/* ipc_receive_char_c --
       Receive an array of integers

   Arguments:
       idcomm       Communication ID
       pos          Position of the integer
       string       String to send
       len_string   Length of the string
*/
void FTNCALL ipc_receive_int_c( int *idcomm, int *pos, char *string, int len_string ) {

    memcpy( string, &comm[*idcomm].data[(*pos)], len_string ) ;
}

#if 0
/* sendStuff --
       In a loop set an integer to increasing values, with a guard to
       tell the receiver it is there
*/
void sendStuff( void ) {

#ifndef LINUX
    HANDLE hfile;
    HANDLE hmap;
    int    *data;
    int    i;

    hfile = CreateFile( "test_mmap.map", GENERIC_WRITE | GENERIC_READ,
                FILE_SHARE_WRITE, NULL, CREATE_ALWAYS,
                FILE_ATTRIBUTE_TEMPORARY, NULL );

    hmap = CreateFileMapping( hfile, NULL, PAGE_READWRITE, 0, 1024, "MAP" );

    data = (int *) MapViewOfFile( hmap, FILE_MAP_ALL_ACCESS, 0, 0, 0 );
#else
    int fd;
    int *data;
    int i;

    printf( "In send\n" );
    fd = open( "test_mmap.map", O_CREAT|O_RDWR, S_IRWXU );
    printf( "FD: %d\n", fd );
    data = (int *) malloc( 1024 );
    write( fd, data, (size_t)1024 );
    free( data );
    data = (int *) mmap( (caddr_t)0, 1024, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0 );
    printf( "Data: %p\n", data );
#endif

    data[0] = 0;
    /* Ready to "send" the data */
    for ( i = 0; i < 10 ; i ++ ) {
        data[1] = i;
        data[0] = 1;

        while ( data[0] == 1 ) {
            Sleep( 100 ) ;
        }
    }
}

/* receiveStuff --
       In a loop get an integer, while checking that there is a new value
*/
void receiveStuff( void ) {

#ifndef LINUX
    HANDLE hfile;
    HANDLE hmap;
    OFSTRUCT of;
    int    *data;
    int    i;

    hfile = (HANDLE) NULL;
    while ( hfile == (HANDLE) NULL ) {
        hfile = CreateFile( "test_mmap.map", GENERIC_WRITE | GENERIC_READ,
                    FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
                    FILE_ATTRIBUTE_TEMPORARY, NULL );
        Sleep( 100 ) ;
    }

    hmap = CreateFileMapping( hfile, NULL, PAGE_READWRITE, 0, 1024, "MAP" );

    data = (int *) MapViewOfFile( hmap, FILE_MAP_ALL_ACCESS, 0, 0, 0 );
#else
    int fd;
    int *data;
    int i;

    fd = open( "test_mmap.map", O_CREAT|O_RDWR, S_IRWXU );
    printf( "FD: %d\n", fd );
    data = (int *) malloc( 1024 );
    write( (size_t)1024, data, fd );
    free( data );
    data = (int *) mmap( (caddr_t)0, 1024, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0 );
    printf( "Data: %p\n", data );
#endif

    /* Ready to "receive" the data */
    for ( i = 0; i < 10 ; i ++ ) {
        while ( data[0] == 0 ) {
            Sleep( 100 ) ;
        }
        printf( "Loop %d: %d\n", i, data[1] );
        data[0] = 0;
    }
}
#endif
