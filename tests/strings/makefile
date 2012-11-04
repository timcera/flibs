# Makefile for "computing" tests and examples
#
# $Id$
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS	=	test_token$(EXEEXT) \
		test_tokenlist$(EXEEXT) \

all:	$(PROGRAMS)

test_token$(OBJEXT):	test_token.f90 tokenize$(OBJEXT)
	$(FC) $(FFLAGS) test_token.f90

tokenize$(OBJEXT):	../../src/strings/tokenize.f90
	$(FC) $(FFLAGS) ../../src/strings/tokenize.f90

test_token$(EXEEXT):	test_token$(OBJEXT) tokenize$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_token$(OBJEXT) tokenize$(OBJEXT)


test_tokenlist$(OBJEXT):	test_tokenlist.f90 tokenlist$(OBJEXT) tokenize$(OBJEXT)
	$(FC) $(FFLAGS) test_tokenlist.f90

tokenlist$(OBJEXT):	../../src/strings/tokenlist.f90
	$(FC) $(FFLAGS) ../../src/strings/tokenlist.f90

test_tokenlist$(EXEEXT):	test_tokenlist$(OBJEXT) tokenlist$(OBJEXT) tokenize$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_tokenlist$(OBJEXT) tokenlist$(OBJEXT) tokenize$(OBJEXT)


clean:	
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)

