/* csqlite.c --
      C wrappers callable from Fortran for the SQLite library

      $Id: csqlite.c,v 1.4 2008/05/04 13:23:56 arjenmarkus Exp $
*/
#define FTNCALL

#include <string.h>
#include "sqlite3.h"

/* Result table for sqlite3_get_table
*/
static char **result;

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

int FTNCALL sqlite3_open_c(
       char     *fname,
       sqlite3 **db,
       int       len_fname
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

int FTNCALL sqlite3_close_c(
       sqlite3 **db
      )
{
   int rc ;

   rc = sqlite3_close(*db);
   return rc ;
}

int FTNCALL sqlite3_do_c(
       sqlite3 **db,
       char *command,
       char *errmsg,
       int   len_command,
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

void FTNCALL sqlite3_finalize_c(
       sqlite3_stmt **stmt
      )
{
   int   rc  ;

   rc = sqlite3_finalize( *stmt ) ;

   return ;
}

void FTNCALL sqlite3_reset_c(
       sqlite3_stmt **stmt
      )
{
   int   rc  ;

   rc = sqlite3_reset( *stmt ) ;

   return ;
}

void FTNCALL sqlite3_step_c(
       sqlite3_stmt **stmt,
       int           *completion
      )
{
   *completion = sqlite3_step( *stmt ) ;

   return ;
}

void FTNCALL sqlite3_errmsg_c(
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

int FTNCALL sqlite3_prepare_c(
       sqlite3      **db,
       char          *command,
       sqlite3_stmt **stmt,
       int            len_command
      )
{
   int   rc   ;
   char *pstr ;

   rc = sqlite3_prepare( *db, command, (-1), stmt, &pstr ) ;

   return rc ;
}

void FTNCALL sqlite3_column_count_c(
       sqlite3_stmt **stmt,
       int           *count
      )
{
   *count = sqlite3_column_count( *stmt ) ;
   return ;
}
void FTNCALL sqlite3_column_name_type_c(
       sqlite3_stmt **stmt,
       int  *colidx,
       char *name,
       char *type,
       int   len_name,
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

int FTNCALL sqlite3_bind_int_c(
       sqlite3_stmt **stmt,
       int           *colidx,
       int           *value
      )
{
   int   rc   ;

   rc = sqlite3_bind_int(*stmt, *colidx, *value ) ;
   return rc ;
}

int FTNCALL sqlite3_bind_double_c(
       sqlite3_stmt **stmt,
       int           *colidx,
       double        *value
      )
{
   int   rc   ;

   rc = sqlite3_bind_double(*stmt, *colidx, *value ) ;
   return rc ;
}

int FTNCALL sqlite3_bind_null_c(
       sqlite3_stmt **stmt,
       int           *colidx
      )
{
   int   rc   ;

   rc = sqlite3_bind_null(*stmt, *colidx ) ;
   return rc ;
}

int FTNCALL sqlite3_bind_text_c(
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

int FTNCALL sqlite3_column_int_c(
       sqlite3_stmt **stmt,
       int           *colidx,
       int           *value
      )
{
   *value = sqlite3_column_int(*stmt, *colidx ) ;
   return 0 ;
}

int FTNCALL sqlite3_column_double_c(
       sqlite3_stmt **stmt,
       int           *colidx,
       double        *value
      )
{
   *value = sqlite3_column_double(*stmt, *colidx ) ;
   return 0 ;
}

int FTNCALL sqlite3_column_text_c(
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

int FTNCALL sqlite3_get_table_1_c(
       sqlite3 **db,
       char *command,
       int  *ncol,
       int  *nrow,
       char *errmsg,
       int   len_command,
       int   len_errmsg
      )
{
   int   rc  ;
   char *msg ;

   rc = sqlite3_get_table(*db, command, &result, nrow, ncol, &msg ) ;
   if ( msg != NULL )
   {
      strncpy( errmsg, msg, len_errmsg ) ;
   }

   return rc ;
}

void FTNCALL sqlite3_get_table_2_c(
       int  *ncol,
       int  *nrow,
       char *result_table,
       int   len_result
      )
{
   int   i   ;
   int   j   ;
   int   k   ;
   int   n   ;

   /* Note: one extra row! */
   for ( j = 0 ; j <= (*nrow) ; j ++ )
   {
      for ( i = 0 ; i < (*ncol) ; i ++ )
      {
         k = i + j*(*ncol) ;

         strncpy( &result_table[k*len_result], result[k], len_result ) ;

         for ( n = strlen(result[k]) ; n < len_result ; n ++ )
         {
            result_table[k*len_result+n] = ' ' ;
         }
      }
   }

   sqlite3_free_table( result ) ;
   result = NULL;

   return ;
}
