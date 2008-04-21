# Makefile for "computing" tests and examples
#
# $Id$
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS	=	test_diff$(EXEEXT) \
		test_decimal$(EXEEXT) \
		test_m_coordinates$(EXEEXT) \

all:	$(PROGRAMS)

test_diff$(OBJEXT):	test_diff.f90 automdiff$(OBJEXT) select_precision$(OBJEXT)
	$(FC) $(FFLAGS) test_diff.f90

automdiff$(OBJEXT):	../../src/computing/automdiff.f90 select_precision$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/computing/automdiff.f90

select_precision$(OBJEXT):	../../src/computing/select_precision.f90
	$(FC) $(FFLAGS) ../../src/computing/select_precision.f90

test_diff$(EXEEXT):	test_diff$(OBJEXT) automdiff$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_diff$(OBJEXT) automdiff$(OBJEXT) select_precision$(OBJEXT)

test_decimal$(OBJEXT):	test_decimal.f90 decimal$(OBJEXT)
	$(FC) $(FFLAGS) test_decimal.f90

decimal$(OBJEXT):	../../src/computing/decimal.f90
	$(FC) $(FFLAGS) ../../src/computing/decimal.f90

test_decimal$(EXEEXT):	test_decimal$(OBJEXT) decimal$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_decimal$(OBJEXT) decimal$(OBJEXT)

test_m_coordinates$(OBJEXT):	test_m_coordinates.f90 m_coordinates$(OBJEXT)
	$(FC) $(FFLAGS) test_m_coordinates.f90

m_coordinates$(OBJEXT):	../../src/computing/m_coordinates.f90
	$(FC) $(FFLAGS) ../../src/computing/m_coordinates.f90

test_m_coordinates$(EXEEXT):	test_m_coordinates$(OBJEXT) m_coordinates$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_m_coordinates$(OBJEXT) m_coordinates$(OBJEXT)

clean:	
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)

