# Options for Intel Fortran - Linux
#
SPACE	=	\

SEP	=	/

FC	=	ifort
FFLAGS_NORMAL	=	-c
FFLAGS_DEBUG	=	-c -g
FFLAGS_OPTIMISE	=	-c -fast

PREPROCESS	=	-fpp
PLATFORM	=	_PLATFORM_OS_LINUX

CC	=	gcc
CFLAGS	=	-c

LD	=	ifort
LDFLAGS_NORMAL	=	
LDFLAGS_DEBUG	=	-g
LDFLAGS_OPTIMISE	=	
LDOUTPUT	=	-o $@

MKLIB	=	ar r
LIBOUT	=

OBJEXT	=	.o
EXEEXT	=	
MODEXT	=	.mod
LIBEXT	=	.a

DELETE	=	rm -f
