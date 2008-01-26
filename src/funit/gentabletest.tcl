# gentabletest.tcl --
#    Generate a test program based on the table specification
#
#    $Id$
#
#    TODO:
#    - RANGES (instead of UNCERTAINTIES)
#    - PRELIMINARIES
#

# data --
#     Gather the fixed code fragments here
#
set keywords {DECLARATIONS CODE ERROR RESULT TABLE TOLERANCE RANGES PRELIMINARIES}
set data(prologue) \
{! Test program
program test_table
    implicit none
    integer, parameter :: wp = kind(PRECISION)
    integer, parameter :: niters = TRIALS
    logical :: error_recognised_
    integer :: luout_
    integer :: lutbl_
    integer :: error_}

set data(precision) 1.0
set data(trials)    100

set data(expected_result) {}

set data(contains) \
{    error_ = 0
    luout_ = 10
    lutbl_ = 11
    open( luout_, file = 'report.out' )
    open( lutbl_, file = 'table.out' )
    call all_tests
    write(luout_,'(a,i5)') 'All tests completed. Number of errors:',error_
contains

logical function not_equal_abs( x, y, margin )
    real(kind=wp) :: x, y, margin
    not_equal_abs = abs(x-y) > margin
end function not_equal_abs

logical function not_equal_rel( x, y, margin )
    real(kind=wp) :: x, y, margin
    not_equal_rel = abs(x-y) > 0.5 * margin * (abs(x)+abs(y))
end function not_equal_rel

subroutine error
    error_recognised_ = .true.
    write(luout_,'(a)') '    Note: error condition correctly recognised'
end subroutine error
subroutine all_tests}

set data(epilogue) {
end subroutine all_tests
end program test_table}

set data(code)         {}
set data(error)        {}
set data(table)        {}
set data(declarations) {}


# generateFromTable --
#     Read the file with the table specification and generate the
#     test code from that
#
# Arguments:
#     tblname      Name of the table file
#
# Result:
#     None
#
# Side effects:
#     Writes the test code
#
proc generateFromTable {tblname} {
    global data

    set infile  [open $tblname r]
    set outname "[file root $tblname].f90"

    #
    # Be careful not to override existing files
    #
    if { [file exists $outname] } {
        return -code error "Output file already exists: $outname"
    }

    set outfile [open $outname w]

    set cont [gets $infile nextline]
    while { $cont >= 0 } {
        #puts "Line: $nextline"
        switch -re $nextline {
            "^ *!" {
                set cont [gets $infile nextline]
            }
            "DECLARATIONS" {
                set cont [readCodeFragment $infile "declarations" nextline]
            }
            "PRELIMINARIES" {
                set cont [readPreliminaries $infile nextline]
            }
            "CODE" {
                set cont [readCodeFragment $infile "code" nextline]
            }
            "ERROR" {
                set cont [readCodeFragment $infile "error" nextline]
            }
            "RESULT" {
                set cont [readResultParameters $infile nextline]
            }
            "RANGES" {
                set cont [readCodeFragment $infile "dummy" nextline]
                #set cont [readRanges $infile nextline]
            }
            "TABLE" {
                set cont [readTable $infile nextline]
            }
            default {
                # Ignore unknown keywords
                set cont [gets $infile nextline]
            }
        }
    }

    close $infile

    #
    # Generate the code from the various pieces
    #

    puts $outfile [string map [list PRECISION $data(precision) TRIALS $data(trials)] $data(prologue)]
    puts $outfile $data(expected_result)
    puts $outfile $data(contains)
    puts $outfile $data(declarations)
    puts $outfile "    write(lutbl_,'(100a12)') '[join $data(variables) ',']'"

    set case 0
    foreach {status entry} $data(table) {
        incr case
        puts $outfile "\n    write(luout_,'(a)') 'Test case: $case'"
        puts $outfile $entry
        puts $outfile $data(code)
        puts $outfile "    write(lutbl_,'(100g12.4)') [join $data(variables) ,]"
        if { $status == 2 } {
            puts $outfile "    error_recognised_ = .false."
            puts $outfile $data(error)
            puts $outfile "    if ( .not. error_recognised_ ) then"
            puts $outfile "        write(luout_,'(a)') '    Failure: error not recognised'"
            puts $outfile "    endif"
        }
        if { $status == 0 } {
            puts $outfile $data(report)
        }
    }

    puts $outfile $data(epilogue)
}


# readCodeFragment --
#     Read a code fragment (skip comments)
#
# Arguments:
#     infile       Input file
#     key          Which key to fill
#     var          Variable to store the next line in
#
# Result:
#     Continue or not
#
# Side effects:
#     Stores the code fragment in the variable data($key)
#
proc readCodeFragment {infile key var} {
    upvar 1 $var nextline
    global data
    global keywords

    while { [gets $infile nextline] >= 0 } {
        if { [lsearch $keywords [string trim $nextline]] >= 0 } {
            return 1
        }
        if { ! [regexp {^ *!} $nextline] } {
            append data($key) "$nextline\n"
        }
    }

    #
    # Apparently the end of the file
    #
    return -1
}


# readResultParameters --
#     Read the information on the result parameters
#
# Arguments:
#     infile       Input file
#     var          Variable to store the next line in
#
# Result:
#     Continue or not
#
# Side effects:
#     Stores the information in the variables data(report),
#     data(result_vars) and data(expected_result)
#
proc readResultParameters {infile var} {
    upvar 1 $var nextline
    global data
    global keywords

    set cont   -1
    set params {}
    while { [gets $infile nextline] >= 0 } {
        if { [lsearch $keywords [string trim $nextline]] >= 0 } {
            set cont 1
            break
        }
        set nextline [lindex [split $nextline !] 0]
        if { [string trim $nextline] != "" } {
            if { [llength $nextline] == 2 } {
                lappend params [string tolower [lindex $nextline 0]] \
                               [string tolower [lindex $nextline 1]]
            } else {
                puts "Error: incorrect result line: $nextline"
            }
        }
    }

    #
    # Transform the list into useable code
    #
    foreach {p margin} $params {
        append data(expected_result) "    real(kind=wp) :: expected_$p\n"

        if { [string first % $margin] >= 0 } {
            set compare "not_equal_rel"
            set margin  [expr {[string map {% ""} $margin]/100.0}]
        } else {
            set compare "not_equal_abs"
        }
        set margin  "${margin}_wp"
        append data(report) "    if ( $compare\($p,expected_$p,$margin) ) then
        write(luout_,'(a,g12.4,a,g12.4)') &
            '    Error: $p = ',$p, ' - expected: ',expected_$p
        error_ = error_ + 1
    endif\n"
    }

    set data(result_vars) $params

    return $cont
}


# readTable --
#     Read the table with test cases
#
# Arguments:
#     infile       Input file
#     var          Variable to store the next line in
#
# Result:
#     Continue or not
#
# Side effects:
#     Stores the information in the variable data(table)
#
proc readTable {infile var} {
    upvar 1 $var nextline
    global data
    global keywords

    set cont  -1
    set table {}
    while { [gets $infile nextline] >= 0 } {
        if { [lsearch $keywords [string trim $nextline]] >= 0 } {
            set cont 1
            break
        }
        if { ! [regexp {^ *!} $nextline] } {
            lappend table [lindex [split $nextline !] 0]
        }
    }

    #
    # Transform the entries into useable code fragments
    #
    set varnames [string tolower [lindex $table 0]]
    set data(variables) $varnames

    foreach values [lrange $table 1 end] {

        set entry {}
        set status 0 ;# Ordinary

        foreach vn $varnames value $values {
            if { $value == "?" } {
                set value  "0.0  ! Undetermined" ;# Should not matter
                if { $status == 0 } {
                    set status 1   ;# No checks necessary
                }
            }
            if { $value == "ERROR" } {
                set value  "0.0  ! Actually ERROR" ;# Should not matter
                set status 2   ;# Check on flagged error condition necessary
            }

            if { [lsearch $data(result_vars) $vn] >= 0 } {
                append entry "    expected_$vn = $value\n"
            } else {
                append entry "    $vn = $value\n"
            }
        }
        lappend data(table) $status $entry
    }

    return $cont
}


# main --
#     Run the program
#
if { [llength $argv] != 1 } {
    puts "Usage: [file tail $argv0] <name-of-table-file>"
} else {
    set tblname [lindex $argv 0]
    generateFromTable $tblname
}

#parray data
