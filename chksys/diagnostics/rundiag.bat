@echo off
rem rundiag.bat --
rem     Run the diagnostics program (uses compile.bat to run the compiler)
rem

if exist verbose del verbose
if /%1 == /-verbose copy rundiag.bat verbose

if not exist compdiag.complete goto loop
    del check.out
    del compdiag.count
    del compdiag.test
    del compdiag.complete
    del compdiag.log    

:loop
    echo compdiag
    compdiag
    call compile check.f90 1>check.out 2>&1
    type check.out
    compdiag
    if exist compdiag.complete goto end
    goto loop

:end
