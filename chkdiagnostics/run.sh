# Simply build and run all the check programs

./compile.sh reportcmp

rm -f reportcmp.out

./compile.sh chk_alloc_pointer_confusion
./compile.sh chk_alloc_string_array
./compile.sh chk_array_access
./compile.sh chk_array_scalar_arg
./compile.sh chk_assign_unallocated
./compile.sh chk_expression_inout
./compile.sh chk_expression_no_intent_out
./compile.sh chk_external_func
./compile.sh chk_func_conditionally_unset
./compile.sh chk_func_unset
./compile.sh chk_hollerith
./compile.sh chk_implicit_interface
./compile.sh chk_implicit_typing
./compile.sh chk_implicit_unseen
./compile.sh chk_incorrect_format
./compile.sh chk_literal_too_precise
./compile.sh chk_long_line_trunc
./compile.sh chk_mismatch_format_data
./compile.sh chk_mismatch_length_arg
./compile.sh chk_mixed_kinds_intrinsic
./compile.sh chk_mixed_precision
./compile.sh chk_pause
./compile.sh chk_pointer_freed_alloc
./compile.sh chk_real_do_variable
./compile.sh chk_real_equals
./compile.sh chk_temporary_array
./compile.sh chk_unalloc_intent_out
./compile.sh chk_unallocated
./compile.sh chk_unallocated_arg
./compile.sh chk_unassociated
./compile.sh chk_undeclared_var
./compile.sh chk_uninit
./compile.sh chk_unset_argument
./compile.sh chk_unset_intent_out
./compile.sh chk_unused_argument
./compile.sh chk_unused_variable
