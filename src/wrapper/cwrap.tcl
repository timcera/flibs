# cwrap.tcl --
#     Program to generate a set of wrapper functions from C header files
#     so that the functions can be used in a Fortran program
#
#     TODO:
#     typedef, enum, ISO-C, c_ptr
#     prototypes without argument names: int f(int,int);
#

# global variables --
#
set typemap {}   ;# Accumulates translation from user-defined types to basic types

set convert {"unsigned int" "int" "unsigned char" "char" "signed char" "char" "short" "int"}

set ignored {WINGDIAPI  "" APIENTRY "" "CONST " "" _cdecl ""
             "CONST84 " "" "CONST84_RETURN " ""} ;# List of ignored keywords (CONST, _cdecl, ...)

# ftype --
#     Translation of C types to corresponding Fortran types
#
array set ftype {int     "integer(c_int)"
                 int*    "integer(c_int), dimension(*)"
                 long    "integer(c_long)"
                 long*   "integer(c_long), dimension(*)"
                 float   "real(c_float)"
                 float*  "real(c_float), dimension(*)"
                 double  "real(c_double)"
                 double* "real(c_double), dimension(*)"
                 char    "character(len=*)"
                 char*   "character(len=*)"
                 void*   "type(c_ptr)"
                 void**  "type(c_ptr)"}

array set isotype {int     "integer(c_int), intent(in), value"
                   int*    "integer(c_int), dimension(*), intent(inout)"
                   long    "integer(c_long), intent(in), value"
                   long*   "integer(c_long), dimension(*), intent(inout)"
                   float   "real(c_float), intent(in), value"
                   float*  "real(c_float), dimension(*), intent(inout)"
                   double  "real(c_double), intent(in), value"
                   double* "real(c_double), dimension(*), intent(inout)"
                   char    "character(len=*), intent(in), value"
                   char*   "character(len=*), intent(inout)"
                   void*   "type(c_ptr), intent(in)"
                   void**  "type(c_ptr), intent(inout)"}


# cwrap --
#     Generate the actual C code and the Fortran interface (if possible)
#
# Arguments:
#     type        Return type of the function
#     name        Name of the function
#     arglist     List of arguments (type and name)
#     args        All other arguments (mainly a consequence of the transformation)
#
# Result:
#     None
#
# Note:
#     Unknown types cause the procedure to write an error message
#     C functions whose interface is ambiguous are left out of the
#     Fortran interface module
#
proc cwrap {type name arglist args} {
    global cout
    global ftnout
    global isoout
    global error

    set error ""
    set fname [string tolower "${name}_"]

    puts "Cwrap: $name"
    puts "Cwrap: $arglist"

    set ftnargs [transformArgList $arglist]
    set body    [setUpBody $type $name $arglist]

    puts $cout "
#ifdef FTN_ALLCAPS
#define $fname [string toupper $name]
#endif

$type STDCALL $fname ( \n    [join $ftnargs ,\n\ \ \ \ ] ) {
$body
}"

    if { $error != "" } {
        puts "Function/routine: $name"
        puts "$error"
    }

    set interface [setUpInterface $type [string tolower $name] $arglist]
    puts $ftnout "\n$interface"

    set interface [setUpIsoCInterface $type [string tolower $name] $name $arglist]
    puts $isoout "\n$interface"
}


# transformToTcl --
#     Transform the C code to a set of Tcl commands for easy processing
#
# Arguments:
#     code          Contents of the C header file
#
# Result:
#     Tcl code that can be evaluated directly
#
# Note:
#     There are rather subtle interactions going on with the
#     substitutions
#
proc transformToTcl {code} {

    set code "\n[removeExternC $code]"            ;# start of all lines easier to detect

    regsub -all {/\*.*?\*/} $code "" code           ;# remove comments
    regsub -all {\\ *\n} $code { } code             ;# continuation of macros
    regsub -all {\n[ \t]*#[ \t]+} $code "\n#" code  ;# normalise #keyword
    regsub -all {#([^\n]+)\n} $code ";#\\1;" code   ;# insert semicolon before and after #keyword
    regsub -all {\n} $code " " code                 ;# commands always end with semicolon
    regsub -all {;} $code "\n" code                 ;# break lines at semicolons
    regsub -all {\{} $code " \{ " code              ;# proper lists
    regsub -all {\}} $code " \} " code              ;# proper lists

    regsub -all {defined *\(([a-zA-Z_0-9]+) *\)} $code "\[defined \\1\]" code
    regsub -all {#if ([^\n]+)} $code "if \{ \\1 \} \{" code
    regsub -all {#elif ([^\n]+)} $code "\} elseif \{ \\1 \} \{" code

#
#   set code [string map {(         " \{"
#                         )         "\} \\ "
#                         \}        "\} "
#                         \{        " \{"
#                         "/*"      ";comment \{"
#                         "*/"      "\}\n"
#                         "const "  " "
#                         "#else"  "\} else \{"
#                         "#endif"  "\}\n"       } $code]
    regsub -all {#ifdef ([a-zA-Z_0-9]+)} $code "if \{ \[defined \\1\] \} \{ " code
    regsub -all {#ifndef ([a-zA-Z_0-9]+)} $code "if \{ ! \[defined \\1\] \} \{ " code
    regsub -all {#define ([^\n]+)\n} $code "define \\1\n\n" code
    regsub -all {#undef ([^\n]+)\n} $code "undef \\1\n\n" code
    regsub -all {#else} $code "\} else \{\n" code
    regsub -all {#endif} $code "\}\n" code
    regsub -all { *\*} $code "* " code
    regsub -all {\* +\*} $code "**" code
    regsub -all {\(} $code " \{ " code
    regsub -all {\)} $code " \} " code

    puts ">>>$code<<<"
    return $code
}


# removeExternC --
#     Remove the "#ifdef __cplusplus extern "C" #endif" sequence and its
#     counterpart
#
# Arguments:
#     code          Contents of the C header file
#
# Result:
#     Slightly altered code
#
proc removeExternC {code} {

    regsub -all {#ifdef +__cplusplus[^#]*#endif} $code {} code
    return $code
}


# transformArgList --
#     Transform the C argument list for the wrapper
#
# Arguments:
#     arglist       String containing the types and names
#
# Result:
#     Argument list for the wrapper
#
proc transformArgList {arglist} {
    global error
    global typemap

    set wraplist {}
    set end      {}
    set carg     0
    foreach arg [split $arglist ,] {
        set arg  [string map $typemap $arg]
        set name [lindex $arg end]
        set type [lindex $arg end-1]

        if { [llength $arg] == 1 } {
            incr carg
            set name "arg__$carg"
            set type [lindex $arg 0]
        }

        switch -- $type {
            "int"    -
            "long"   -
            "float"  -
            "double" {
                lappend wraplist "$type* $name"
            }
            "int*"    -
            "long*"   -
            "float*"  -
            "double*" -
            "void*"   -
            "void**"  {
                lappend wraplist "$type $name"
            }
            "char"    -
            "char*"   {
                lappend wraplist "$type $name"
                lappend end      "int len__$name"
            }
            ""        -
            "void"    {
                # Nothing
            }
            default {
                append error "\n    $arg: conversion to/from Fortran not supported"
            }
        }

    }

    return [concat $wraplist $end]
}


# setUpBody --
#     Construct the body of the wrapper
#
# Arguments:
#     type          Type of value to be returned
#     name          Name of the original function
#     arglist       String containing the types and names
#
# Result:
#     Body for the wrapper
#
proc setUpBody {type name arglist} {
    global error
    global typemap

    set type [string map $typemap $type]

    if { $type != "void" } {
        set body   "    $type result__;\n"
        set call   "    result__ = $name ("
        set return "    return result__;"
    } else {
        set body   ""
        set call   "    $name ("
        set return "    return;"
    }
    set wraplist {}
    set carg     0
    foreach arg [split $arglist ,] {
        set arg  [string map $typemap $arg]
        set name [lindex $arg end]
        set type [lindex $arg end-1]

        if { [llength $arg] == 1 } {
            incr carg
            set name "arg__$carg"
            set type [lindex $arg 0]
        }

        switch -- $type {
            "char"   -
            "int"    -
            "long"   -
            "float"  -
            "double" {
                lappend wraplist "*$name"
            }
            "char*"   -
            "int*"    -
            "long*"   -
            "float*"  -
            "double*" -
            "void*"   -
            "void**"  {
                lappend wraplist "$name"
            }
            "void"    {
                # Nothing
            }
            default {
                # Nothing!
            }
        }

        set body "$call [join $wraplist ,\ ] );\n$return"
    }

    return $body
}


# setUpInterface --
#     Construct the Fortran 90/95 interface
#
# Arguments:
#     type          Type of value to be returned
#     fname         Name as known to Fortran
#     arglist       String containing the types and names
#
# Result:
#     Body for the wrapper
#
proc setUpInterface {type fname arglist} {
    global error
    global ftype
    global typemap

    set type [string map $typemap $type]

    if { $type != "void" } {
        set body   "    $ftype($type) function $fname ("
        set end    "    end function $fname"
    } else {
        set body   "    subroutine $fname ("
        set end    "    end subroutine $fname"
    }
    set wraplist  {}
    set ftnargs   {}
    set ambiguous 0
    set carg      0
    foreach arg [split $arglist ,] {
        set arg  [string map $typemap $arg]
        set name [lindex $arg end]
        set type [lindex $arg end-1]

        if { [llength $arg] == 1 } {
            incr carg
            set name "arg__$carg"
            set type [lindex $arg 0]
        }

        switch -- $type {
            "char"   -
            "char*"  -
            "int"    -
            "long"   -
            "float"  -
            "double" -
            "void*"  -
            "void**" {
                lappend wraplist "$ftype($type) :: $name"
                lappend ftnargs  "$name"
            }
            "int*"    -
            "long*"   -
            "float*"  -
            "double*" {
                set ambiguous 1
                lappend wraplist "$ftype($type) :: $name"
                lappend ftnargs  "$name"
            }
            "void" {
                # Nothing
            }
            default {
                # Nothing!
            }
        }

    }

    if { $ambiguous } {
        set body "    ! Ambiguous interface: scalars or arrays?\n$body"
    }
    set body "$body [join $ftnargs ,\ ] )\n        [join $wraplist \n\ \ \ \ \ \ \ \ ]\n$end"

    return $body
}


# setUpIsoCInterface --
#     Construct the Fortran 2003 interface, using the standard ISO C bindings
#
# Arguments:
#     type          Type of value to be returned
#     fname         Name as known to Fortran
#     cname         Original C name
#     arglist       String containing the types and names
#
# Result:
#     Body for the wrapper
#
proc setUpIsoCInterface {type fname cname arglist} {
    global error
    global isotype
    global typemap

    set type [string map $typemap $type]

    if { $type != "void" } {
        set body   "    function $fname ("
        set result "result(result__)
        use iso_c_binding
        [lindex [split $isotype($type) ,] 0] :: result__"
        set end    "    end function $fname"
    } else {
        set body   "    subroutine $fname ("
        set result "\n        use iso_c_binding"
        set end    "    end subroutine $fname"
    }
    set wraplist  {}
    set ftnargs   {}
    set ambiguous 0
    set carg      0
    foreach arg [split $arglist ,] {
        set arg  [string map $typemap $arg]
        set name [lindex $arg end]
        set type [lindex $arg end-1]

        if { [llength $arg] == 1 } {
            incr carg
            set name "arg__$carg"
            set type [lindex $arg 0]
        }

        switch -- $type {
            "char"   -
            "char*"  -
            "int"    -
            "long"   -
            "float"  -
            "double" -
            "void*"  {
                lappend wraplist "$isotype($type) :: $name"
                lappend ftnargs  "$name"
            }
            "int*"    -
            "long*"   -
            "float*"  -
            "double*" {
                set ambiguous 1
                lappend wraplist "$isotype($type) :: $name"
                lappend ftnargs  "$name"
            }
            "void" {
            }
            default {
                # Nothing!
            }
        }

    }

    if { $ambiguous } {
        set body "    ! Ambiguous interface: scalars or arrays?\n$body"
    }
    set body "$body [join $ftnargs ,\ ] ) &
        bind(C,name=\"$cname\") $result
        [join $wraplist \n\ \ \ \ \ \ \ \ ]\n$end"

    return $body
}


# prologue --
#     Write the prologue code for the wrapper
#
# Arguments:
#     module        Name of the module
#     names         Names of the header files
#
# Result:
#     None
#
proc prologue {module names} {
    global cout
    global ftnout
    global isoout

    puts $cout \
"/* Wrapper derived from the header file(s) $names
*/"

    foreach name $names {
        puts $cout "#include \"$name\""
    }

    puts $cout \
"#ifdef WIN32
#define STDCALL stdcall__
#else
#define STDCALL
#endif
#include \"cfstring.h\"
"

    foreach out [list $ftnout $isoout] {
        puts $out \
"! Interfaces for wrapper routines (derived from the header file(s) $names)
!
module $module
    use iso_c_binding
    implicit none

interface"
    }

}


# epilogue --
#     Write the epilogue code for the wrapper
#
# Arguments:
#     None
#
# Result:
#     None
#
proc epilogue {module} {
    global cout
    global ftnout
    global isoout
    global declout

    close $declout
    set infile [open "${module}_decl.f90" r]
    set contents [read $infile]
    close $infile

    file delete "${module}_decl.f90"

    foreach out [list $ftnout $isoout] {
        puts $out \
"end interface

    !
    ! Parameters (macros) from the C header file(s) - if any
    !
$contents

end module"
    }
}


# defined --
#     Check if a macro has been defined or not
#
# Arguments:
#     macro         Name of the macro
#
# Result:
#     None
#
proc defined {macro} {
    global macros

    info exists macros($macro)
}


# define --
#     Define a macro
#
# Arguments:
#     macro         Name of the macro
#     args          String of values assigned to it
#
# Result:
#     None
#
proc define {macro args} {
    global macros
    global declout

    set macros($macro) $args

    #
    # Check if the macro represents a value. If so, write to
    # the Fortran module as a parameter
    #
    # TODO: solve the subtleness of #define A "A" - the quotes disappear in Tcl!
    #
    if { [llength $args] == 1 } {
        set value [lindex $args 0]
        if { [string is integer -strict $value] } {
            puts $declout "    integer(c_int), parameter :: $macro = [expr {$value+0}] ! $value"
        } elseif { [string is double -strict $value] } {
            puts $declout "    real(c_double), parameter :: $macro = [expr {$value+0.0}] ! $value"
        } else {
            puts $declout "    character(len=[string length $value]), parameter :: $macro = \"$value\""
        }
    }
}


# undef --
#     Undefine a macro
#
# Arguments:
#     macro         Name of the macro
#
# Result:
#     None
#
# Note:
#     This has no equivalent on the Fortran side
#
proc undef {macro} {
    global macros

    unset macros($macro)
}


# enum --
#     Transform enumerations
#
# Arguments:
#     list          List of enumerated names (and possibly values)
#     args          Any names for this enumeration
#
# Result:
#     None
#
proc enum {list args} {
    global declout

    set value -1
    foreach e [split $list ,] {
        if { [string first = $e] < 0 } {
            set name [string trim $e]
            incr value
        } else {
            regexp { *([a-zA-Z_0-9]+) *= *([a-zA-Z_0-9]+)} $e ==> name value
        }

        puts $declout "    integer, parameter :: $name = $value"
    }
}


# typedef --
#     Handle type definitions
#
# Arguments:
#     args          List of all arguments
#
# Result:
#     None
#
# Note:
#     At this moment only simple typedefs are treated!
#
proc typedef {args} {
    global typemap

    #
    # Simple case: two arguments
    #
    if { [llength $args] == 2 } {
        set basic   [lindex $args 0]
        set newname [lindex $args 1]

        lappend typemap $newname $basic

        proc $newname {name arglist args} "cwrap $basic \$name \$arglist"

    } else {
        #
        # More complex cases
        #
        puts "Typedef: [lindex $args 0] ... [lindex $args end] - not treated yet"
    }
}


# handleArgs --
#     Handle the command-line arguments
#
# Arguments:
#     argv          List of arguments
#
# Result:
#     List of parameters (name of the module, names of the header files)
#
proc handleArgs {argv} {

    set module  ""
    set isvalue 0
    set names   {}

    foreach arg $argv {
        if { $isvalue } {
            set isvalue 0
            set $var    $arg
        } else {
            switch -- $arg {
                "-module" {
                    set isvalue 1
                    set var     "module"
                }
                default {
                    set newnames [glob -nocomplain $arg]
                    if { [llength $newnames] > 0 } {
                        set names [concat $names $newnames]
                    } else {
                        puts "No file(s) by that name: $arg"
                    }
                }
            }
        }
    }

    if { $module == "" } {
        set module [file root [file tail [lindex $names 0]]]
    }
    regsub -all {[^a-zA-Z0-9_]} $module "" module

    return [list $module $names]
}


# comment, void, ... --
#     Auxiliary procedures
#
proc extern {args} {
    # No op: definition of external data
}
proc EXTERN {type args} {
    eval cwrap $type $args
}

foreach type {char int long float double void void*} {
    proc $type {name arglist} [string map [list TYPE $type] {
        cwrap TYPE $name $arglist
    }]
}

proc unknown {cmdname args} {
    puts "Unknown type: $cmdname"
    puts "Prototype:   $args"
    return
}

# main --
#     Get the program going:
#     Options:
#     -module name   Set the name of the module (otherwise: cwrapper)
#     All others     Names of the header files to be treated
#
foreach {module names} [handleArgs $argv] {break}

set macros(__TCL__) ""
set cout    [open "${module}_wrap.c"  w]
set ftnout  [open "${module}_mod.f90" w]
set isoout  [open "${module}_iso.f90" w]
set declout [open "${module}_decl.f90" w]

prologue $module $names

foreach name $names {
    set infile [open $name r]
    set contents [read $infile]
    close $infile

    set contents [string map $convert $contents]
    set contents [string map $ignored $contents]

    #puts [transformToTcl $contents]
    eval [transformToTcl $contents]
}

epilogue $module
