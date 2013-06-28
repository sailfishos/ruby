#!/bin/bash

[ $# -ge 1 ] || {
    cat > /dev/null
    exit 0
}

case $1 in
-P|--provides)
    shift
    RPM_BUILD_ROOT="$1"
    while read possible
    do
	case "$possible" in
	*.gemspec)
	    possible=${possible##*/}
	    possible=${possible%.gemspec} 
	    echo "$possible" | sed -e 's,^\(.*\)-,rubygem(\1) >= ,'
	    ;;
	esac
    done
    ;;
-R|--requires)
    while read possible ; do
	case "$possible" in
	*.gemspec)
	    echo "$possible" | sed -ne 's,.*/gems/,ruby(abi) >= ,; s,/.*,,p'
	    ;;
	esac
    done
    ;;
esac
exit 0
