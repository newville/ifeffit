#
#  ifeffit interface in tcl
#
#  M Newville  Aug 10, 1999
# 
#
# catch { load /usr/local/share/ifeffit/tcl/libifeffit_tcl.so ifeffit }
catch { load ./ifeffit_tcl.so ifeffit}

proc ifeffit  {cmd} {iff_exec $cmd}
proc put_scalar {name val} {ifeffit  "set   $name = $val"}
proc put_string {name val} {ifeffit  "set \$$name = $val"}

proc get_scalar {name} {
    set ptr [ptrcreate double]
    iff_get_scalar $name  $ptr
    set ret [ptrvalue  $ptr]
    ptrfree $ptr
    return $ret
}

proc get_string {name} {
    set str "                                                                                                                                            "
    set len [iff_get_string $name $str]
    return [string trim $str]
}

proc get_array {name} {
    set ptr  [ptrcreate double 0 16384]
    set npts [iff_get_array $name $ptr]
    set list {}
    for {set i 0} {$i < $npts} {incr i +1} {
	lappend list [ptrvalue $ptr $i]
    }
    ptrfree $ptr
    return $list
}

proc put_array {name list} {
    set npts   [llength $list]
    set ptr    [ptrcreate double 0 $npts]
    set p_npts [ptrcreate int]
    ptrset $p_npts $npts
    for {set i 0} {$i < $npts} {incr  i +1} {
	ptrset  $ptr [lindex $list $i] $i
    }
    set ret [iff_put_array $name $p_npts $ptr]
    ptrfree $p_npts
    ptrfree $ptr
    return $ret
}


proc get_echo {} {
    set str "                                                                                                                              " 
    set len [iff_get_echo $str]
    return [string trim $str]
}

