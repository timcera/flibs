# Makefile for "fsqlite" library
#
# NOTE:
# The source code uses the C-Fortran interface features of Fortran 2003.
# This takes care of the naming and calling conventions.
#
# $Id: makefile,v 1.1 2008/05/04 13:23:56 arjenmarkus Exp $
#
include ../../config/config.mk
include ../../config/options.mk

LIBRARIES	=	libfsqlite$(LIBEXT) \

#
# Use the correct macro for interfacing between Fortran and C and
# add the directory where the Sqlite3 header file can be found
#
#DEFINES	=	-DDBL_UNDERSCORE -I../../../sqlite
#DEFINES	=	-DLOWERCASE -I../../../sqlite
#DEFINES	=	-I../../../sqlite


all:	$(LIBRARIES)

libfsqlite$(LIBEXT):	fsqlite$(OBJEXT) csqlite$(OBJEXT) sqlite3$(OBJEXT)
	$(MKLIB) $(LIBOUT)libfsqlite$(LIBEXT) fsqlite$(OBJEXT) csqlite$(OBJEXT) sqlite3$(OBJEXT)

fsqlite$(OBJEXT):	fsqlite.f90
	$(FC) $(FFLAGS) fsqlite.f90

csqlite$(OBJEXT):	csqlite.c
	echo "$(CC) $(CFLAGS) csqlite.c $(DEFINES)"
	$(CC) $(CFLAGS) csqlite.c $(DEFINES)

sqlite3$(OBJEXT):	sqlite3.c sqlite3.h
	$(CC) $(CFLAGS) sqlite3.c $(DEFINES)

clean:	
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)
	$(DELETE) *$(LIBEXT)

