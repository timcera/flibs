@echo off
rem Simply build and run all the check programs

call .\compile.bat reportcmp

del reportcmp.out

call .\compile.bat chk_alloc_pointer_confusion
call .\compile.bat chk_alloc_string_array
call .\compile.bat chk_array_access
call .\compile.bat chk_array_scalar_arg
call .\compile.bat chk_assign_unallocated
call .\compile.bat chk_expression_inout
call .\compile.bat chk_expression_no_intent_out
call .\compile.bat chk_external_func
call .\compile.bat chk_func_conditionally_unset
call .\compile.bat chk_func_unset
call .\compile.bat chk_hollerith
call .\compile.bat chk_implicit_interface
call .\compile.bat chk_implicit_typing
call .\compile.bat chk_implicit_unseen
call .\compile.bat chk_incorrect_format
call .\compile.bat chk_literal_too_precise
call .\compile.bat chk_long_line_trunc
call .\compile.bat chk_mismatch_format_data
call .\compile.bat chk_mismatch_length_arg
call .\compile.bat chk_mixed_kinds_intrinsic
call .\compile.bat chk_mixed_precision
call .\compile.bat chk_pause
call .\compile.bat chk_pointer_freed_alloc
call .\compile.bat chk_real_do_variable
call .\compile.bat chk_real_equals
call .\compile.bat chk_temporary_array
call .\compile.bat chk_unalloc_intent_out
call .\compile.bat chk_unallocated
call .\compile.bat chk_unallocated_arg
call .\compile.bat chk_unassociated
call .\compile.bat chk_undeclared_var
call .\compile.bat chk_uninit
call .\compile.bat chk_unset_argument
call .\compile.bat chk_unset_intent_out
call .\compile.bat chk_unused_argument
call .\compile.bat chk_unused_variable
