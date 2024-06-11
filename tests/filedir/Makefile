# Makefile for "filedir" tests and examples
#
# $Id: makefile,v 1.11 2013-12-30 07:16:26 arjenmarkus Exp $
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS	=	test_m_fileunit$(EXEEXT) \
	test_m_filedir$(EXEEXT) \
	test_m_vfile$(EXEEXT) \

FFLAGS = $(FFLAGS) -I../../tests/support
FFLAGS = $(FFLAGS) -I../../src/datastructures
FFLAGS = $(FFLAGS) $(PREPROCESS)
FFLAGS = $(FFLAGS) -D$(PLATFORM)
FFLAGS = $(FFLAGS) -D_VSTRING_ALLOCATABLE -D_VSTRINGLIST_ALLOCATABLE -D_VFILE_GETCWD_FUNCTION -D_PLATFORM_CHDIR_FUNCTION
FFLAGS = $(FFLAGS) -D_FS_GETCWD_FUNCTION -D_PLATFORM_FORTRAN_2003

all:	$(PROGRAMS)


test_m_fileunit$(EXEEXT):	test_m_fileunit$(OBJEXT)  m_fileunit$(OBJEXT) m_platform$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_m_fileunit$(OBJEXT)  m_fileunit$(OBJEXT) m_platform$(OBJEXT)

test_m_fileunit$(OBJEXT):	test_m_fileunit.f90  m_fileunit$(OBJEXT) m_platform$(OBJEXT)
	$(FC) $(FFLAGS) test_m_fileunit.f90

m_fileunit$(OBJEXT):	../../src/filedir/m_fileunit.f90
	$(FC) $(FFLAGS) ../../src/filedir/m_fileunit.f90

m_platform$(OBJEXT):	../../src/platform/m_platform.f90
	$(FC) $(FFLAGS) ../../src/platform/m_platform.f90

test_m_filedir$(EXEEXT):	test_m_filedir$(OBJEXT)  m_filedir$(OBJEXT) m_platform$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_m_filedir$(OBJEXT)  m_filedir$(OBJEXT) m_platform$(OBJEXT)

test_m_filedir$(OBJEXT):	test_m_filedir.f90  m_filedir$(OBJEXT) m_platform$(OBJEXT)
	$(FC) $(FFLAGS) test_m_filedir.f90

m_filedir$(OBJEXT):	../../src/filedir/m_filedir.f90
	$(FC) $(FFLAGS) ../../src/filedir/m_filedir.f90

test_m_vfile$(EXEEXT):	test_m_vfile$(OBJEXT)  m_vfile$(OBJEXT) m_vstring$(OBJEXT) m_vstringformat$(OBJEXT) m_vstringlist$(OBJEXT) m_vstrplatform$(OBJEXT) m_platform$(OBJEXT) m_fileunit$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_m_vfile$(OBJEXT)  m_vfile$(OBJEXT) m_vstring$(OBJEXT) m_vstringformat$(OBJEXT) m_vstringlist$(OBJEXT) m_vstrplatform$(OBJEXT) m_platform$(OBJEXT) m_fileunit$(OBJEXT)

test_m_vfile$(OBJEXT):	test_m_vfile.f90  m_vfile$(OBJEXT) m_vstring$(OBJEXT) m_vstringformat$(OBJEXT) m_vstringlist$(OBJEXT) m_vstrplatform$(OBJEXT) m_platform$(OBJEXT) m_fileunit$(OBJEXT)
	$(FC) $(FFLAGS) test_m_vfile.f90

m_vfile$(OBJEXT):	../../src/filedir/m_vfile.f90 m_vstring$(OBJEXT) m_vstringformat$(OBJEXT) m_vstringlist$(OBJEXT) m_vstrplatform$(OBJEXT) m_platform$(OBJEXT) m_fileunit$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/filedir/m_vfile.f90

m_vstring$(OBJEXT):	../../src/strings/m_vstring.f90
	$(FC) $(FFLAGS) ../../src/strings/m_vstring.f90

m_vstringformat$(OBJEXT):	../../src/strings/m_vstringformat.f90 m_vstring$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/strings/m_vstringformat.f90

m_vstringlist$(OBJEXT):	../../src/strings/m_vstringlist.f90 m_vstring$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/strings/m_vstringlist.f90

m_vstrplatform$(OBJEXT):	../../src/platform/m_vstrplatform.f90 m_platform$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/platform/m_vstrplatform.f90



clean:
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)
