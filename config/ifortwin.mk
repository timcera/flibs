# Options for Intel Fortran - Windows
#
SPACE	=	\

SEP	=	/

FC	=	ifort
FFLAGS_NORMAL	=	/c
FFLAGS_DEBUG	=	/c /debug
FFLAGS_OPTIMISE	=	/c /fast

PREPROCESS=	=	/fpp /D_FS_INTEL_FORTRAN_PORTABILITY_ROUTINES /D_FS_SYSTEM_FUNCTIONS
PLATFORM	=	_PLATFORM_OS_WINDOWS_NT

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
