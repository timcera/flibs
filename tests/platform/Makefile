# Makefile for "platform" tests and examples
#
# $Id: makefile,v 1.11 2013-12-30 07:16:26 arjenmarkus Exp $
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS	=	test_m_platform$(EXEEXT) \
	test_m_vstrplatform$(EXEEXT) \

FFLAGS +=-I../../tests/support
FFLAGS +=$(PREPROCESS)
FFLAGS +=-D$(PLATFORM)
FFLAGS +=-D_VSTRING_ALLOCATABLE -D_PLATFORM_FORTRAN_2003

all:	$(PROGRAMS)


test_m_platform$(EXEEXT):	test_m_platform$(OBJEXT)  m_platform$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_m_platform$(OBJEXT)  m_platform$(OBJEXT)

test_m_platform$(OBJEXT):	test_m_platform.f90  m_platform$(OBJEXT)
	$(FC) $(FFLAGS) test_m_platform.f90

m_platform$(OBJEXT):	../../src/platform/m_platform.f90
	$(FC) $(FFLAGS) ../../src/platform/m_platform.f90

test_m_vstrplatform$(EXEEXT):	test_m_vstrplatform$(OBJEXT)  m_vstrplatform$(OBJEXT) m_platform$(OBJEXT) m_vstring$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_m_vstrplatform$(OBJEXT)  m_vstrplatform$(OBJEXT) m_platform$(OBJEXT) m_vstring$(OBJEXT)

test_m_vstrplatform$(OBJEXT):	test_m_vstrplatform.f90  m_vstrplatform$(OBJEXT) m_platform$(OBJEXT) m_vstring$(OBJEXT)
	$(FC) $(FFLAGS) test_m_vstrplatform.f90

m_vstrplatform$(OBJEXT):	../../src/platform/m_vstrplatform.f90 m_platform$(OBJEXT) m_vstring$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/platform/m_vstrplatform.f90

m_vstring$(OBJEXT):	../../src/strings/m_vstring.f90
	$(FC) $(FFLAGS) ../../src/strings/m_vstring.f90



clean:
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)
	$(DELETE) *.log
