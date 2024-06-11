# Options for gfortran - Linux version
#
# Maybe define LDOUTPUT as LDOUTPUT=-o $@
#
SPACE	=	\

SEP	=	/

FC	=	gfortran
FFLAGS_NORMAL	=	-c
FFLAGS_DEBUG	=	-c -g
FFLAGS_OPTIMISE	=	-c -O

PREPROCESS	=	-cpp -D_FS_SYSTEM_FUNCTIONS
PLATFORM	=	_PLATFORM_OS_LINUX

CC	=	gcc
CFLAGS	=	-c

LD	=	gfortran
LDFLAGS_NORMAL	=	
LDFLAGS_DEBUG	=	-g
LDFLAGS_OPTIMISE	=	
#LDOUTPUT	=	-o$(SPACE)
LDOUTPUT	=	-o $@

MKLIB	=	ar r
LIBOUT	=
EXTRALIBS	=	-lpthread -ldl

OBJEXT	=	.o
EXEEXT	=	
MODEXT	=	.mod
LIBEXT	=	.a

DELETE	=	rm -f
