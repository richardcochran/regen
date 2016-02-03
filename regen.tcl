#!/usr/bin/tclsh
#
# Copyright (C) 2009 OMICRON electronics GmbH
# Copyright (C) 2015 Richard Cochran <richardcochran@gmail.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

proc Debug {} {
	parray ::attribute_name
	parray ::attribute_offset
	parray ::attribute_type
	parray ::attribute_width
	parray ::children
	parray ::parent
}

proc FixBit {p} {
	if {$::reverse_bits} {
		return [expr 31 - $p]
	} else {
		return $p
	}
}

proc FixField {p w} {
	if {$::reverse_bits} {
		return [expr 31 - ($p + $w - 1)]
	} else {
		return $p
	}
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

proc Puts {fd str} {
	puts $fd [string trimright $str]
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
			Puts $fd ""
			Puts $fd "#define ${name}\t[format $fmt $offset]$cmt"
		} register {
			Puts $fd "#define ${name}\t[format $fmt $offset]$cmt"
		} bit {
			set pos [FixBit $pos]
			Puts $fd "#define ${name}\t(1<<$pos)$cmt"
		} field {
			set mask [Mask $width]
			set pos [FixField $pos $width]
			Puts $fd "#define ${name}_SHIFT\t($pos)$cmt"
			Puts $fd "#define ${name}_MASK\t($mask)"
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
			if {[llength $::children($obj)]} {
				Puts $fd ""
				Puts $fd "/* Bit definitions for the ${name} register */"
			}
		} bit {
			set pos [FixBit $pos]
			set tmp "(1<<$pos)"
			Puts $fd "#define [format $::nfmt $name] [format $::bfmt $tmp]$cmt"
		} field {
			set mask [Mask $width]
			set pos [FixField $pos $width]
			set tmp "($pos)"
			Puts $fd "#define [format $::nfmt ${name}_SHIFT] [format $::bfmt $tmp]$cmt"
			Puts $fd "#define [format $::nfmt ${name}_MASK] ($mask)"
		}
	}
	foreach child $::children($obj) {
		VisitBits $fd $child
	}
}

namespace eval DefineStyle {

	proc BeginBase {fd name fmt offset cmt} {
		Puts $fd ""
		Puts $fd "#define [format $::nfmt $name] [format $fmt $offset]$cmt"
	}

	proc EndBase {fd name fmt offset cmt} {
	}

	proc Register {fd name fmt offset width cmt} {
		Puts $fd "#define [format $::nfmt $name] [format $fmt $offset]$cmt"
	}
}

namespace eval StructureStyle {

	proc BeginBase {fd name fmt offset cmt} {
		set ::register_offset 0
		set ::padding_count 0
		Puts $fd ""
		Puts $fd "struct [string tolower $name] \{ $cmt"
	}

	proc EndBase {fd name fmt offset cmt} {
		Puts $fd "\};"
	}

	proc Register {fd name fmt offset width cmt} {
		set bytes [expr $width / 8]
		set coff  [expr $::register_offset + $bytes]
		if {$offset != $coff} {
			set pad [expr $offset - $coff]
			if {$pad > 0} {
				Puts $fd "\tu8  res$::padding_count\[$pad\];"
			}
			incr ::padding_count
		}
		set name [format $::nfmt [string tolower ${name}\; ]]
		Puts $fd "\tu$width $name $cmt"
		set ::register_offset $offset
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
			${::style}::BeginBase $fd $name $fmt $offset $cmt
		} register {
			${::style}::Register $fd $name $fmt $offset $width $cmt
		}
	}
	foreach child $::children($obj) {
		VisitRegisters $fd $child
	}
	switch -exact -- $type {
		base {
			${::style}::EndBase $fd $name $fmt $offset $cmt
		}
	}
}

proc Usage {} {
	puts ""
	puts "usage: regen.tcl \[options\] input_file"
	puts ""
	puts "  -d  use #define style (default)"
	puts "  -h  show this message"
	puts "  -r  reverse the bit numbering"
	puts "  -s  use structure style"
	puts ""
}

# Main program

if {$argc < 1 || $argc > 3} {
	Usage
	exit -1
}
if {$argc == 1 && [lindex $argv 0] == "-h"} {
	Usage
	exit 0
}
set ::reverse_bits 0
set ::style DefineStyle
set infile [lindex $argv end]
for {set i 0} {$i < [expr $argc - 1]} {incr i} {
	switch -exact -- [lindex $argv $i] {
		-d {
			set ::style DefineStyle
		} -h {
			Usage
			exit 0
		} -r {
			set ::reverse_bits 1
		} -s {
			set ::style StructureStyle
		} default {
			Usage
			exit -1
		}
	}
}
set fd [open $infile "r"]
set all [read $fd]
close $fd

set ::objcount 0
set root [New "" void]
ParsePairs $root $all
set ::nfmt [NameFormat]
set ::bfmt "%-7s"

#ShowObject $root 0
set id [file rootname [file tail $infile]]
set fd stdout
puts $fd "/* ${id}_reg.h"
puts $fd " * Generated by [file tail [info script]] on [clock format [clock seconds]]"
puts $fd " * https://github.com/richardcochran/regen"
puts $fd " */"
puts $fd "#ifndef HAVE_[string toupper $id]_REGISTERS"
puts $fd "#define HAVE_[string toupper $id]_REGISTERS"
VisitRegisters $fd $root
VisitBits $fd $root
puts $fd ""
puts $fd "#endif"
