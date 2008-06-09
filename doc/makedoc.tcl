#
# A script to generate the html documentation from the 
# doctools man files.
#

package require doctools

proc formatfile {manfile} {
    ::doctools::new mdt
    mdt configure -file $manfile
    mdt configure -format html
    set handle [open $manfile r]
    set content [read -nonewline $handle]
    close $handle
    set htmlcontent [mdt format $content]
    set htmlfile [computefilename $manfile .html]
    set handle [open $htmlfile w]
    puts -nonewline $handle $htmlcontent
    close $handle
    mdt destroy
    return $htmlfile
}

proc computefilename {filename newextension} {
    set lastdot [string last "." $filename]
    incr lastdot -1
    set newname [string range $filename 0 $lastdot]
    append newname $newextension
    return $newname
}

proc processall {} {
    set manfiles [glob "*.man"]
    foreach filename $manfiles {
        set isuptodate [isuptodate $filename]
        if {$isuptodate==0} then {
            puts "> Updating $filename..."
            formatfile $filename
        } else {
            puts "> Up-to-date: $filename "
        }
    }
    return ""
}
proc isuptodate {manfile} {
    set htmlfile [computefilename $manfile .html]
    set time1 [file mtime $manfile]
    set time2 [file mtime $htmlfile]
    set result [expr {$time1< $time2}]
    return $result
}
processall

