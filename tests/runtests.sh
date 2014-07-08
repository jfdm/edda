#!/bin/bash

die() {
	echo "$1" >&2
	exit 1
}

clean_up() {
	rm -f *.ibc test json output
}

clean_up

echo "compiling edda tests..."
idris Test.idr -p lightyear -p edda -o test || die "* could not compile tests *"

echo "compiled OK, running edda tests..."
timeout 5s ./test > output || die "* test failed or timed out *"

if diff output expected; then
	echo "### everything PASS ###"
	clean_up
	exit 0
else
	echo "### something FAIL ###"
	#clean_up
	exit 1
fi
