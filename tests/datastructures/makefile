# Makefile for "datastructures" tests and examples
#
# $Id: makefile,v 1.11 2013-12-30 07:16:26 arjenmarkus Exp $
#
include ../../config/config.mk
include ../../config/options.mk

FFLAGS +=-I../../src/datastructures
FFLAGS +=-I../../tests/support
FFLAGS +=$(PREPROCESS)

PROGRAMS	=	test_vectors$(EXEEXT) \
	test_btree$(EXEEXT) \
	test_dict$(EXEEXT) \
	test_list$(EXEEXT) \
	test_sets$(EXEEXT) \
	two_lists$(EXEEXT) \
	test_queue$(EXEEXT) \
	test_qsortarray_template$(EXEEXT) \
	qsort_test$(EXEEXT) \
	test_balanced_tree$(EXEEXT) \
	test_balanced_tree_string$(EXEEXT) \

# For reasons I do not understand making the program test_mem_pool under
# Cygwin all of a sudden invokes gcc instead of gfortran to link it.
# So keep this program for now :(.
# 	test_mem_pool$(EXEEXT) \

all:	$(PROGRAMS)


test_vectors$(EXEEXT):	test_vectors$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_vectors$(OBJEXT)

test_vectors$(OBJEXT):	test_vectors.f90
	$(FC) $(FFLAGS) test_vectors.f90


test_btree$(EXEEXT):	test_btree$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_btree$(OBJEXT)

test_btree$(OBJEXT):	test_btree.f90
	$(FC) $(FFLAGS) test_btree.f90


test_dict$(EXEEXT):	test_dict$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_dict$(OBJEXT)

test_dict$(OBJEXT):	test_dict.f90
	$(FC) $(FFLAGS) test_dict.f90


test_list$(EXEEXT):	test_list$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_list$(OBJEXT)

test_list$(OBJEXT):	test_list.f90
	$(FC) $(FFLAGS) test_list.f90


test_mempool$(EXEEXT):	test_mempool$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_mempool$(OBJEXT)

test_mem_pool$(OBJEXT):	test_mem_pool.f90
	$(FC) $(FFLAGS) test_mem_pool.f90


test_sets$(EXEEXT):	test_sets$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_sets$(OBJEXT)

test_sets$(OBJEXT):	test_sets.f90
	$(FC) $(FFLAGS) test_sets.f90


two_lists$(EXEEXT):	two_lists$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) two_lists$(OBJEXT)

two_lists$(OBJEXT):	two_lists.f90
	$(FC) $(FFLAGS) two_lists.f90


test_queue$(EXEEXT):	test_queue$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_queue$(OBJEXT)

test_queue$(OBJEXT):	test_queue.f90
	$(FC) $(FFLAGS) test_queue.f90


test_qsortarray_template$(EXEEXT):	test_qsortarray_template$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_qsortarray_template$(OBJEXT)

test_qsortarray_template$(OBJEXT):	test_qsortarray_template.f90
	$(FC) $(FFLAGS) test_qsortarray_template.f90


qsort_test$(EXEEXT):	qsort_test$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) qsort_test$(OBJEXT)

qsort_test$(OBJEXT):	qsort_test.f90
	$(FC) $(FFLAGS) qsort_test.f90

test_balanced_tree$(EXEEXT):	test_balanced_tree$(OBJEXT)
	$(LD) $(LDFLAGS) test_balanced_tree$(OBJEXT)

test_balanced_tree$(OBJEXT):	test_balanced_tree.f90 ../../src/datastructures/balanced_tree.f90
	$(FC) $(FFLAGS) test_balanced_tree.f90

test_balanced_tree_string$(EXEEXT):	test_balanced_tree_string$(OBJEXT)
	$(LD) $(LDFLAGS) test_balanced_tree$(OBJEXT)

test_balanced_tree_string$(OBJEXT):	test_balanced_tree_string.f90 ../../src/datastructures/balanced_tree_string.f90
	$(FC) $(FFLAGS) test_balanced_tree.f90




clean:
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)
