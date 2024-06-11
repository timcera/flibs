# Makefile for "dynlib" tests and examples
#
# $Id: makefile,v 1.11 2013-12-30 07:16:26 arjenmarkus Exp $
#
include ../../config/config.mk
include ../../config/options.mk

#
# NOTE: for the moment only for gfortran on Windows
#
SHLEXT	=	.dll
SHARED	=	-shared -Wl,-o$@
SYSTEMLIB	=	win_gnu_dynlib

PROGRAMS	=	test_dynlib$(EXEEXT) \
	test_lib$(SHLEXT) \


all:	$(PROGRAMS)


test_dynlib$(EXEEXT):	test_dynlib$(OBJEXT) dynlib$(OBJEXT) $(SYSTEMLIB)$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_dynlib$(OBJEXT) dynlib$(OBJEXT) $(SYSTEMLIB)$(OBJEXT)

test_dynlib$(OBJEXT):	test_dynlib.f90 dynlib$(OBJEXT) $(SYSTEMLIB)$(OBJEXT)
	$(FC) $(FFLAGS) test_dynlib.f90


dynlib$(OBJEXT):	../../src/dynlib/dynlib.f90 $(SYSTEMLIB)$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/dynlib/dynlib.f90

$(SYSTEMLIB)$(OBJEXT):	../../src/dynlib/$(SYSTEMLIB).f90
	$(FC) $(FFLAGS) ../../src/dynlib/$(SYSTEMLIB).f90


test_lib$(SHLEXT):	test_lib$(OBJEXT)
	$(LD) $(LDFLAGS) $(SHARED) test_lib$(OBJEXT)

test_lib$(OBJEXT):	test_lib.f90
	$(FC) $(FFLAGS) $(SHARED) test_lib.f90


clean:
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)
