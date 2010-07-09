
namespace eval IntervalTreeTest {
    variable maxKey 1e4
    variable maxGrowSize 200

    proc QueryList { listName low high } {
	upvar $listName list
	set result {}
	foreach interval $list {
	    set lowInt [lindex $interval 0]
	    set highInt [lindex $interval 1]
	    if {( ($lowInt <= $low)  && ($highInt >= $low) )  ||
	    ( ($lowInt <= $high) && ($highInt >= $high))  ||
	    ( ($lowInt >= $low)  && ($lowInt <= $high)) } {
		lappend result $interval
	    }
	}
	return $result
    }
    
    proc CreateRandomInterval { } {
	variable maxKey
	set low [expr round((rand() * $maxKey))]
	set high [expr round((rand() * $maxKey))]
	if {$low > $high} {set low [expr - $low]}
	return [list $low $high]
    }
    
    proc CheckQuery { tree listName } {
	upvar $listName list
	set interval [CreateRandomInterval]
	set treeResult [lsort [eval intTreeQuery $tree $interval]]
	set listResult [lsort [eval QueryList list $interval]]
	if {[string compare $treeResult $listResult]} {
	    return [list $interval $treeResult $listResult]
	} else {
	    return {}
	}
    }
    
    proc Add { tree listName } {
	upvar $listName list
	set interval [CreateRandomInterval]
	lappend list $interval
	eval intTreeAdd $tree $interval
	return
    }
    
    proc Remove {tree listName} {
	variable maxGrowSize
	upvar $listName list
	set index [expr round(rand() * $maxGrowSize) % [llength $list]]
	set interval [lindex $list $index]
	eval intTreeRemoveNode $tree $interval
	set list [lreplace $list $index $index]
	return
    }
    
    # offset = 0 makes tree/list grow, offset = 1 makes it shrink.
    proc AddRemoveOrQuery { tree listName offset} {
	upvar $listName list
	set i [expr (round(rand() * 64 ) % 4) + $offset]
	switch $i {
	    0
	    -  
	    1	{ Add $tree list}
	    2       { 
		set result [CheckQuery $tree list]
		if {[llength $result]} {
		    error "CheckQuery failed with result '$result'\n"
		}
	    }
	    3 
	    -
	    4 	 { 
		if {[llength $list]} {
		    Remove $tree list
		} 
	    }
	    default { error "Unreachable part of switch statement seen ($i)!"}
	}
	puts -nonewline "[llength $list] "
	flush stdout
    }
    
    proc GrowThenShrinkCheck {tree listName } {
	variable maxGrowSize
	upvar $listName list

	puts "\nStarting GrowThenShrinkCheck for IntervalTree\n"
	while { [llength $list] < $maxGrowSize } {
	    AddRemoveOrQuery $tree list 0
	}
	while { [llength $list] } {
	    AddRemoveOrQuery $tree list 1
	}
    }
    
    proc DoAllIntervalTreeTests { } {
	intTreeCreate junk
	set ::intTreeList {}
	GrowThenShrinkCheck junk ::intTreeList
	intTreeDelete junk
    }
}



namespace eval RedBlackTreeTest {
    variable maxKey 1e4
    variable maxGrowSize 200

    proc QueryList { listName low high } {
	upvar $listName list
	set result {}
	foreach key $list {
	    if { ($low <= $key) && ($key <= $high) } {
		lappend result $key
	    }
	}
	return $result
    }
    
    proc CreateRandomEntry { } {
	variable maxKey
	return [expr round((rand() * $maxKey))]
    }
    
    proc CheckQuery { tree listName } {
	upvar $listName list
	set low [CreateRandomEntry]
	set high [CreateRandomEntry]
	set treeResult [lsort -integer [rbTreeQuery $tree $low $high]]
	set listResult [lsort -integer [QueryList list $low $high]]
	if {[string compare $treeResult $listResult]} {
	    return [list $low $high $treeResult $listResult]
	} else {
	    return {}
	}
    }
    
    proc Add { tree listName } {
	upvar $listName list
	set key [CreateRandomEntry]
	lappend list $key
	rbTreeAdd $tree $key
	return
    }
    
    proc Remove {tree listName} {
	variable maxGrowSize
	upvar $listName list
	set index [expr round(rand() * $maxGrowSize) % [llength $list]]
	set key [lindex $list $index]
	rbTreeRemoveNode $tree $key $key
	set list [lreplace $list $index $index]
	return
    }
    
    # offset = 0 makes tree/list grow, offset = 1 makes it shrink.
    proc AddRemoveOrQuery { tree listName offset} {
	upvar $listName list
	set i [expr (round(rand() * 64 ) % 4) + $offset]
	switch $i {
	    0
	    -  
	    1	{ Add $tree list}
	    2       { 
		set result [CheckQuery $tree list]
		if {[llength $result]} {
		    error "CheckQuery failed with result '$result'\n"
		}
	    }
	    3 
	    -
	    4 	 { 
		if {[llength $list]} {
		    Remove $tree list
		} 
	    }
	    default { error "Unreachable part of switch statement seen ($i)!"}
	}
	puts -nonewline "[llength $list] "
	flush stdout
    }
    
    proc GrowThenShrinkCheck {tree listName} {
	variable maxGrowSize
	upvar $listName list

	puts "\nStarting GrowThenShrinkCheck for RedBlackTree\n"
	while { [llength $list] < $maxGrowSize } {
	    AddRemoveOrQuery $tree list 0
	}
	while { [llength $list] } {
	    AddRemoveOrQuery $tree list 1
	}
    }
    
    proc DoAllRedBlackTreeTests { } {
	rbTreeCreate junk
	set ::rbTreeList {}
	GrowThenShrinkCheck junk ::rbTreeList 
	rbTreeDelete junk
    }
}

::RedBlackTreeTest::DoAllRedBlackTreeTests
::IntervalTreeTest::DoAllIntervalTreeTests
puts "\nTests passed.  No errors found.\n"
