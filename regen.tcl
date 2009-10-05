#!/usr/bin/tclsh

proc Debug {} {
    parray ::attribute_name
    parray ::attribute_offset
    parray ::attribute_type
    parray ::attribute_width
    parray ::children
    parray ::parent
}

proc New {parent type} {
    set obj obj[incr ::objcount]
    set ::attribute_comment($obj)  ""
    set ::attribute_format($obj)   "0x%x"
    set ::attribute_name($obj)     ""
    set ::attribute_offset($obj)   ""
    set ::attribute_pos($obj)      ""
    set ::attribute_type($obj)     $type
    set ::attribute_width($obj)    ""
    set ::children($obj)           ""
    set ::parent($obj)             $parent
    return $obj
}

proc Mask {w} {
    set sum 0
    for {set i 0} {$i < $w} {incr i} {
	set sum [expr $sum | (1<<$i)]
    }
    return [format 0x%x $sum]
}

proc NameFormat {} {
    # Compute the name format, for pretty printing.
    set max 0
    foreach n [array names ::attribute_name] {
	set len [string length $::attribute_name($n)]
	if {$len > $max} {
	    set max $len
	}
    }
    incr max 6
    return %-${max}s
}

proc ParsePairs {parent pairs} {
    set len [llength $pairs]
    for {set i 0} {$i < $len} {incr i 2} {
	set key [lindex $pairs $i]
	set val [lindex $pairs [expr 1+$i]]
	if {1 == [llength $val] || "comment" == $key} {
	    # This is a simple attribute.
	    set ::attribute_${key}($parent) $val
	} else {
	    # This is a complex object.
	    set ref [New $parent $key]
	    lappend ::children($parent) $ref
	    ParsePairs $ref $val
	    SetFormat $ref
	}
    }
}

proc ShowObject {obj d} {
    for {set i 0} {$i < $d} {incr i} {
	puts -nonewline "\t"
    }
    puts -nonewline "$obj "
    puts -nonewline "name=$::attribute_name($obj) "
    puts -nonewline "type=$::attribute_type($obj) "
    puts ""
    foreach child $::children($obj) {
	ShowObject $child [expr 1+$d]
    }
}

proc SetFormat {obj} {
    set fmt 0x%x
    set width $::attribute_width($obj)
    switch -exact -- $width {
	8 {
	    set fmt 0x%02x
	} 16 {
	    set fmt 0x%04x
	} 32 {
	    set fmt 0x%08x
	} 64 {
	    set fmt 0x%016xLL
	}
    }
    set ::attribute_format($obj) $fmt
}

proc FormatComment {obj} {
    set cmt $::attribute_comment($obj)
    if {"" != $cmt} {
	set cmt [format " /* %s */" $cmt]
    }
    return $cmt
}

proc Visit {fd obj} {
    set cmt    [FormatComment $obj]
    set fmt    $::attribute_format($obj)
    set name   $::attribute_name($obj)
    set offset $::attribute_offset($obj)
    set pos    $::attribute_pos($obj)
    set type   $::attribute_type($obj)
    set width  $::attribute_width($obj)
    switch -exact -- $type {
	base {
	    puts $fd ""
	    puts $fd "#define ${name}\t[format $fmt $offset]$cmt"
	} register {
	    puts $fd "#define ${name}\t[format $fmt $offset]$cmt"
	} bit {
	    puts $fd "#define ${name}\t(1<<$pos)$cmt"
	} field {
	    set mask [Mask $width]
	    puts $fd "#define ${name}_SHIFT\t(1<<$pos)$cmt"
	    puts $fd "#define ${name}_MASK\t($mask<<$pos)"
	}
    }
    foreach child $::children($obj) {
	Visit $child
    }
}

proc VisitBits {fd obj} {
    set cmt    [FormatComment $obj]
    set fmt    $::attribute_format($obj)
    set name   $::attribute_name($obj)
    set offset $::attribute_offset($obj)
    set pos    $::attribute_pos($obj)
    set type   $::attribute_type($obj)
    set width  $::attribute_width($obj)
    switch -exact -- $type {
	base {
	} register {
	    puts $fd ""
	    puts $fd "/* Bit definitions for the ${name} register */"
	} bit {
	    puts $fd "#define [format $::nfmt $name] (1<<$pos)$cmt"
	} field {
	    set mask [Mask $width]
	    puts $fd "#define [format $::nfmt ${name}_SHIFT] (1<<$pos)$cmt"
	    puts $fd "#define [format $::nfmt ${name}_MASK] ($mask<<$pos)"
	}
    }
    foreach child $::children($obj) {
	VisitBits $fd $child
    }
}

proc VisitRegisters {fd obj} {
    set cmt    [FormatComment $obj]
    set fmt    $::attribute_format($obj)
    set name   $::attribute_name($obj)
    set offset $::attribute_offset($obj)
    set pos    $::attribute_pos($obj)
    set type   $::attribute_type($obj)
    set width  $::attribute_width($obj)
    switch -exact -- $type {
	base {
	    puts $fd ""
	    puts $fd "#define [format $::nfmt $name] [format $fmt $offset]$cmt"
	} register {
	    puts $fd "#define [format $::nfmt $name] [format $fmt $offset]$cmt"
	}
    }
    foreach child $::children($obj) {
	VisitRegisters $fd $child
    }
}

# Main program

if {$argc != 1} {
    puts "need an input file"
    exit -1
}
set infile [lindex $argv 0]
set fd [open $infile "r"]
set all [read $fd]
close $fd

set ::objcount 0
set root [New "" void]
ParsePairs $root $all
set ::nfmt [NameFormat]

#ShowObject $root 0
VisitRegisters stdout $root
VisitBits stdout $root
puts ""