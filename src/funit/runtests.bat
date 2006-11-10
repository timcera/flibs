@echo off
rem runtests.bat --
rem     DOS batch file to control a program that uses funit
rem     Name of the program: first argument
rem
rem     $Id$
rem
if exist runtests.log del runtests.log
echo ALL >funit.run

:run
%1 %2 %3 %4 %5 %6 %7 %8 %9 >runtests.out 2>runtests.err
type runtests.out >>runtests.log
type runtests.err >>runtests.log
if exist funit.lst goto run

del funit.run
del runtests.out
del runtests.err
