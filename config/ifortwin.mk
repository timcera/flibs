# Options for Intel Fortran - Windows
#
SPACE	=	\

SEP	=	/

FC	=	ifort
FFLAGS_NORMAL	=	/c
FFLAGS_DEBUG	=	/c /debug
FFLAGS_OPTIMISE	=	/c /fast

CC	=	cl
CFLAGS	=	/c

LD	=	ifort
LDFLAGS_NORMAL	=	
LDFLAGS_DEBUG	=	/debug
LDFLAGS_OPTIMISE	=	/fast
LDOUTPUT	=	/exe:$@

MKLIB	=	lib
LIBOUT	=	/out:
EXTRALIBS	=

OBJEXT	=	.obj
EXEEXT	=	.exe
MODEXT	=	.mod
LIBEXT	=	.lib

DELETE	=	del /q
