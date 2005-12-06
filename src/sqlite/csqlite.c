/* csqlite.c --
      C wrappers callable from Fortran for the SQLite library
*/
#ifdef WIN32
#define FTNCALL __stdcall
#define INBETWEEN
#define sqlite3_open_c_                SQLITE3_OPEN_C
#define sqlite3_close_c_               SQLITE3_CLOSE_C
#define sqlite3_do_c_                  SQLITE3_DO_C
#define sqlite3_finalize_c_            SQLITE3_FINALIZE_C
#define sqlite3_reset_c_               SQLITE3_RESET_C
#define sqlite3_step_c_                SQLITE3_STEP_C
#define sqlite3_insert_c_              SQLITE3_INSERT_C
#define sqlite3_prepare_c_             SQLITE3_PREPARE_C
#define sqlite3_column_count_c_        SQLITE3_COLUMN_COUNT_C
#define sqlite3_column_name_type_c_    SQLITE3_COLUMN_NAME_TYPE_C
#define sqlite3_errmsg_c_              SQLITE3_ERRMSG_C
#define sqlite3_bind_int_c_            SQLITE3_BIND_INT_C
#define sqlite3_bind_null_c_           SQLITE3_BIND_NULL_C
#define sqlite3_bind_double_c_         SQLITE3_BIND_DOUBLE_C
#define sqlite3_bind_text_c_           SQLITE3_BIND_TEXT_C
#define sqlite3_column_int_c_          SQLITE3_COLUMN_INT_C
#define sqlite3_column_double_c_       SQLITE3_COLUMN_DOUBLE_C
#define sqlite3_column_text_c_         SQLITE3_COLUMN_TEXT_C
#else
#define FTNCALL
#endif

#include <string.h>
#include "sqlite3.h"

static int callback(void *NotUsed, int argc, char **argv, char **azColName){
  /*
  int i;
  for(i=0; i<argc; i++){
    printf("%s = %s\n", azColName[i], argv[i] ? argv[i] : "NULL");
  }
  printf("\n");
  */
  return 0;
}

int FTNCALL sqlite3_open_c_(
       char *fname,
#ifdef INBETWEEN
       int   len_fname,
#endif
       sqlite3 **db
#ifndef INBETWEEN
      ,int   len_fname
#endif
      )
{
   int rc ;

   rc = sqlite3_open(fname, db);

   if ( rc != 0 )
   {
      sqlite3_close(*db);
   }
   return rc ;
}

int FTNCALL sqlite3_close_c_(
       sqlite3 **db
      )
{
   int rc ;

   rc = sqlite3_close(*db);
   return rc ;
}

int FTNCALL sqlite3_do_c_(
       sqlite3 **db,
       char *command,
#ifdef INBETWEEN
       int   len_command,
#endif
       char *errmsg,

#ifndef INBETWEEN
       int   len_command,
#endif
       int   len_errmsg
      )
{
   int   rc  ;
   char *msg ;

   rc = sqlite3_exec(*db, command, callback, 0, &msg ) ;
   if ( msg != NULL )
   {
      strncpy( errmsg, msg, len_errmsg ) ;
   }
   return rc ;
}

void FTNCALL sqlite3_finalize_c_(
       sqlite3_stmt **stmt
      )
{
   int   rc  ;

   rc = sqlite3_finalize( *stmt ) ;

   return ;
}

void FTNCALL sqlite3_reset_c_(
       sqlite3_stmt **stmt
      )
{
   int   rc  ;

   rc = sqlite3_reset( *stmt ) ;

   return ;
}

void FTNCALL sqlite3_step_c_(
       sqlite3_stmt **stmt,
       int           *completion
      )
{
   *completion = sqlite3_step( *stmt ) ;

   return ;
}

void FTNCALL sqlite3_errmsg_c_(
       sqlite3 **db,
       char *errmsg,
       int   len_errmsg
      )
{
   char *pstr ;

   pstr = sqlite3_errmsg( *db ) ;
   strncpy( errmsg, pstr, len_errmsg ) ;

   return ;
}

int FTNCALL sqlite3_prepare_c_(
       sqlite3      **db,
       char *command,
#ifdef INBETWEEN
       int   len_command,
#endif
       sqlite3_stmt **stmt
#ifndef INBETWEEN
      ,int   len_command
#endif
      )
{
   int   rc   ;
   char *pstr ;

   rc = sqlite3_prepare( *db, command, (-1), stmt, &pstr ) ;

   return rc ;
}

void FTNCALL sqlite3_column_count_c_(
       sqlite3_stmt **stmt,
       int           *count
      )
{
   *count = sqlite3_column_count( *stmt ) ;
   return ;
}
void FTNCALL sqlite3_column_name_type_c_(
       sqlite3_stmt **stmt,
       int  *colidx,
       char *name,
#ifdef INBETWEEN
       int   len_name,
#endif
       char *type,

#ifndef INBETWEEN
       int   len_name,
#endif
       int   len_type
      )
{
   int   rc   ;
   char *pstr ;

   pstr = sqlite3_column_name(*stmt, *colidx ) ;
   strncpy( name, pstr, len_name ) ;
   name[len_name-1] = '\0' ;
   pstr = sqlite3_column_decltype(*stmt, *colidx ) ;
   strncpy( type, pstr, len_type ) ;
   type[len_type-1] = '\0' ;
   return ;
}

int FTNCALL sqlite3_bind_int_c_(
       sqlite3_stmt **stmt,
       int           *colidx,
       int           *value
      )
{
   int   rc   ;

   rc = sqlite3_bind_int(*stmt, *colidx, *value ) ;
   return rc ;
}

int FTNCALL sqlite3_bind_double_c_(
       sqlite3_stmt **stmt,
       int           *colidx,
       double        *value
      )
{
   int   rc   ;

   rc = sqlite3_bind_double(*stmt, *colidx, *value ) ;
   return rc ;
}

int FTNCALL sqlite3_bind_null_c_(
       sqlite3_stmt **stmt,
       int           *colidx
      )
{
   int   rc   ;

   rc = sqlite3_bind_null(*stmt, *colidx ) ;
   return rc ;
}

int FTNCALL sqlite3_bind_text_c_(
       sqlite3_stmt **stmt,
       int           *colidx,
       char          *text,
       int            len_text
      )
{
   int   rc   ;

   rc = sqlite3_bind_text(*stmt, *colidx, text, len_text,
           SQLITE_TRANSIENT ) ;
   return rc ;
}

int FTNCALL sqlite3_column_int_c_(
       sqlite3_stmt **stmt,
       int           *colidx,
       int           *value
      )
{
   *value = sqlite3_column_int(*stmt, *colidx ) ;
   return 0 ;
}

int FTNCALL sqlite3_column_double_c_(
       sqlite3_stmt **stmt,
       int           *colidx,
       double        *value
      )
{
   *value = sqlite3_column_double(*stmt, *colidx ) ;
   return 0 ;
}

int FTNCALL sqlite3_column_text_c_(
       sqlite3_stmt **stmt,
       int           *colidx,
       char          *text,
       int            len_text
      )
{
   char *pstr ;

   pstr = sqlite3_column_text(*stmt, *colidx ) ;
   strncpy( text, pstr, len_text ) ;
   return 0 ;
}
