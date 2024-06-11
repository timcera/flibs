# Makefile for building the "generic" modules and demo programs
#
# $Id$
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS        =       main_filter_file$(EXEEXT) \
                        main_filter_station$(EXEEXT) \
                        main_filter_array$(EXEEXT)

all:    $(PROGRAMS)

main_filter_file$(OBJEXT): main_filter_file.f90 collection_file$(OBJEXT) basic_types$(OBJEXT)
	$(FC) $(FFLAGS) main_filter_file.f90

main_filter_station$(OBJEXT): main_filter_station.f90 collection_file$(OBJEXT) basic_types$(OBJEXT)
	$(FC) $(FFLAGS) main_filter_station.f90

main_filter_array$(OBJEXT): main_filter_array.f90 collection_array$(OBJEXT) basic_types$(OBJEXT)
	$(FC) $(FFLAGS) main_filter_array.f90

collection_file$(OBJEXT): collection_file.f90 collection_file_body.f90 collection_generic.f90 collection_implementation.f90 basic_types$(OBJEXT)
	$(FC) $(FFLAGS) collection_file.f90

collection_array$(OBJEXT): collection_array.f90 collection_array_body.f90 collection_generic.f90 collection_implementation.f90 basic_types$(OBJEXT)
	$(FC) $(FFLAGS) collection_array.f90

basic_types$(OBJEXT): basic_types.f90
	$(FC) $(FFLAGS) basic_types.f90

main_filter_file$(EXEEXT): main_filter_file$(OBJEXT) collection_file$(OBJEXT) basic_types$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) main_filter_file$(OBJEXT) collection_file$(OBJEXT) basic_types$(OBJEXT)

main_filter_station$(EXEEXT): main_filter_station$(OBJEXT) collection_file$(OBJEXT) basic_types$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) main_filter_station$(OBJEXT) collection_file$(OBJEXT) basic_types$(OBJEXT)

main_filter_array$(EXEEXT): main_filter_array$(OBJEXT) collection_array$(OBJEXT) basic_types$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) main_filter_array$(OBJEXT) collection_array$(OBJEXT) basic_types$(OBJEXT)


clean:
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)

