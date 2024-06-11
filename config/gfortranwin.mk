# Options for gfortran - Windows (Cygwin MinGW) version
#
# Maybe define LDOUTPUT as LDOUTPUT=-o $@
#
SPACE	=	\

SEP	=	/

FC	=	gfortran
FFLAGS_NORMAL	=	-c
FFLAGS_DEBUG	=	-c -g
FFLAGS_OPTIMISE	=	-c -O

PREPROCESS	=	-cpp
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
EXTRALIBS	=

OBJEXT	=	.o
EXEEXT	=	
MODEXT	=	.mod
LIBEXT	=	.a


DELETE	=	rm -f
