@echo off
rem Compile, link and run with the necessary compile options
echo .
echo ---------------------
echo %1 ...
echo .
ifort %1.f90 -warn:all -check:all -stand -standard-semantics >_comp_.out 2>&1
type _comp_.out
.\reportcmp.exe compiler %1
echo .
if not exist %1.exe goto skip
    .\%1.exe >_runexe_.out 2>&1
    .\reportcmp.exe output %1
    type _runexe_.out
goto end
:skip
    echo "No program %1"
:end
