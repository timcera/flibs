Checking compiler features
--------------------------

The collection of small programs in this directory was written to check
if the compiler of choice supports particular features or how they work
out:

- Features of the current Fortran standard (Fortran 2008).

- Run-time behaviour, such as what happens if the program reads a file
  with different line-endings than native to the platform.

- Extensions to the standards that may or may not be required for a
  particular program to function properly.

It is not exhaustive nor will it ever be. It should, however, give a
first start.


Running the checks
------------------

The checks are run, depending on the platform, via a batch file,
"chkfeatures.bat" or a shell script "chkfeatures":

- The program "buildscript" (as contained in the source file
  "buildscript.f90") prepares a simple batch file or shell script that
  builds and runs the actual checking programs

- Building is done via a batch file "compile.bat" or a shell script
  "compile", which is expected to build the program from the given
  source file. For instance, for gfortran, it contains the command:

  gfortran -o $1.exe $1.f90 $2

  where $1 is the name (without extension) of the source file to be
  compiled. The extension ".exe" is required (it makes it easy to
  distinguish the programs and to remove any leftovers at the start)

- The results are collected in a simple report "features.out". No
  interpretation beyond that contained in the source files is done.

- If a program cannot be compiled, then an appropriate message is
  printed instead.


Preparation
-----------

For all platforms: the batch file "compile.bat" and the shell script
"compile" are responsible for compiling and linking the program.

You will need to edit this file for the appropriate command for your
compiler. You may also need to set up the environment - make sure the
compiler is in the path, license information is available and so on.

On Linux:

Make sure that the files "chkfeatures" and "compile" have the execute
permission:

chmod +x chkfeatures compile

ought to do that.


Additional information
----------------------

The program "buildscript" takes the input file "buildscript.set" to set
up a small batch file/shell script that runs the various checking
programs. This means that you can add your own checking programs as
well.

The format of this file is very simple:

- A line starting with an at sign (@) should contain the name of the
  source file (without extension).

- It should be followed by one or more lines of text to be printed when
  the compilation fails as an aid to the user to interpret this failure.

- Then an empty line to separate each block.


MINLOC/MAXLOC:
Greetings Arjen Markus,

Comment by jimdempseyatthecove:
From IVF 17.0 document:
If array has size zero, or every element of mask has the value .FALSE., the value of the result is controlled by compiler option assume [no]old_maxminloc, which can set the value of the result to either 1 or 0.
and
old_maxminloc
MAXLOC and MINLOC return 1 when given an empty array as an argument or every element of the mask is false.
IOW use the -noold_maxminloc option to return 0.
BTW this is an example of an option switch that is not instructive as to what/how to use it. (same with the ifort /? information)
Jim Dempsey


Original thread:
Up until 2014 version of IVF compilers, if MINLOC or MAXLOC had a MASK argument conformable with the array being searched for the minimum or maximum value, the result would be a 0 if no element of the array satisfied the condition in the corresponding element of MASK. I haven't checked since 2014, but in trying to determine why a code that has previously worked is giving absurd results, I narrowed it down to the fact the result of such an inquiry is now 1 (one) instead of 0 (zero). I checked the latest manual for 19.0.1.144 and there is a mention of this in green font (don't know the significance).
My comments/questions are:
1. This is very inconvenient since now one has to test whether MASK has any true entries before using MASK in MINLOC/MAXLOC.
2. A return value of 1 cannot be logically tested since the first element of the array could actually have the minimum or maximum value.
3. Where within the various release notes can one find such changes in compiler behavior documented for easy reference?
4. What is the setting in Microsoft Visual Studio that will allow default to the old behavior?
program main
  implicit none
  integer(kind = 4) :: n
  integer(kind = 4) :: indx(4)
  real(kind = 4) :: x(4)
  !
  n = 4
  x = [-3.0, 2.0, 4.0, -10.0]
  !
  indx = [-1,0,1,1]
  print *, 'Case 1 (some true elements in mask): ', minval(minloc(x, indx == 1))
  !
  indx = [-1,0,-1,0]
  print *, 'Case 2 (all elements of mask are false): ', minval(minloc(x, indx == 1))
  !
  read *
end program main
Results:
 Case 1 (some true elements in mask):            4
 Case 2 (all elements of mask are false):        1



Read more: https://software.intel.com/en-us/comment/1931779#comment-1931779

This is an automatic message from Intel� Software.
You can unsubscribe at https://software.intel.com/en-us/user/342522/notifications/subscription.




This email is an automated notification. Please do not reply to this message; replies are not monitored.
________________________________________
Mail-ID: e00ce0e6d61ea5796f1f33c3ac36c627

