# Makefile for "string manipulation" tests and examples
#
# $Id$
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS	=	test_token$(EXEEXT) \
		test_tokenlist$(EXEEXT) \
		text_tst$(EXEEXT) \
		test_textstr_func$(EXEEXT) \

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


textstr$(OBJEXT):	../../src/strings/textstr.f90
	$(FC) $(FFLAGS) ../../src/strings/textstr.f90

text_tst$(OBJEXT):	text_tst.f90 textstr$(OBJEXT)
	$(FC) $(FFLAGS) text_tst.f90

text_tst$(EXEEXT):	text_tst$(OBJEXT) textstr$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) text_tst$(OBJEXT) textstr$(OBJEXT)


textstr_func$(OBJEXT):	../../src/strings/textstr_func.f90
	$(FC) $(FFLAGS) ../../src/strings/textstr_func.f90

test_textstr_func$(OBJEXT):	test_textstr_func.f90 textstr_func$(OBJEXT)
	$(FC) $(FFLAGS) test_textstr_func.f90

test_textstr_func$(EXEEXT):	test_textstr_func$(OBJEXT) textstr_func$(OBJEXT) textstr_func$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_textstr_func$(OBJEXT) textstr_func$(OBJEXT) textstr$(OBJEXT)


clean:	
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)

