# Makefile for "reporting" tests and examples
#
# $Id: makefile,v 1.11 2013-12-30 07:16:26 arjenmarkus Exp $
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS	=	test_m_exception$(EXEEXT) \
	test_m_throwable$(EXEEXT) \
	test_m_logger$(EXEEXT) \
	test_m_assert$(EXEEXT) \
	test_m_multilog$(EXEEXT) \

FFLAGS	+=	-D_ASSERT_ENABLE

all:	$(PROGRAMS)


test_m_exception$(EXEEXT):	test_m_exception$(OBJEXT)  m_exception$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_m_exception$(OBJEXT)  m_exception$(OBJEXT)

test_m_exception$(OBJEXT):	test_m_exception.f90  m_exception$(OBJEXT)
	$(FC) $(FFLAGS) test_m_exception.f90

m_exception$(OBJEXT):	../../src/reporting/m_exception.f90
	$(FC) $(FFLAGS) ../../src/reporting/m_exception.f90

test_m_throwable$(EXEEXT):	test_m_throwable$(OBJEXT)  m_throwable$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_m_throwable$(OBJEXT)  m_throwable$(OBJEXT)

test_m_throwable$(OBJEXT):	test_m_throwable.f90  m_throwable$(OBJEXT)
	$(FC) $(FFLAGS) test_m_throwable.f90

m_throwable$(OBJEXT):	../../src/reporting/m_throwable.f90
	$(FC) $(FFLAGS) ../../src/reporting/m_throwable.f90

test_m_logger$(EXEEXT):	test_m_logger$(OBJEXT)  m_logger$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_m_logger$(OBJEXT)  m_logger$(OBJEXT)

test_m_logger$(OBJEXT):	test_m_logger.f90  m_logger$(OBJEXT)
	$(FC) $(FFLAGS) test_m_logger.f90

m_logger$(OBJEXT):	../../src/reporting/m_logger.f90
	$(FC) $(FFLAGS) ../../src/reporting/m_logger.f90

test_m_assert$(EXEEXT):	test_m_assert$(OBJEXT)  m_assert$(OBJEXT) m_exception$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_m_assert$(OBJEXT)  m_assert$(OBJEXT) m_exception$(OBJEXT)

test_m_assert$(OBJEXT):	test_m_assert.f90  m_assert$(OBJEXT) m_exception$(OBJEXT)
	$(FC) $(FFLAGS) test_m_assert.f90

m_assert$(OBJEXT):	../../src/reporting/m_assert.f90 m_exception$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/reporting/m_assert.f90

test_m_multilog$(EXEEXT):	test_m_multilog$(OBJEXT)  m_multilog$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_m_multilog$(OBJEXT)  m_multilog$(OBJEXT)

test_m_multilog$(OBJEXT):	test_m_multilog.f90  m_multilog$(OBJEXT)
	$(FC) $(FFLAGS) test_m_multilog.f90

m_multilog$(OBJEXT):	../../src/reporting/m_multilog.f90
	$(FC) $(FFLAGS) ../../src/reporting/m_multilog.f90



clean:
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)
	$(DELETE) *.log
	$(DELETE) *.out
