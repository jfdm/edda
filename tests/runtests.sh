#!/bin/bash

die() {
	echo "$1" >&2
	exit 1
}

clean_up() {
	rm -f *.ibc output eddatests
}

clean_up

echo "compiling edda tests..."
idris Main.idr -i src -p lightyear -p edda -o eddatests || die "* could not compile tests *"

echo "compiled OK, running edda tests..."
timeout 5s ./eddatests > output || die "* test failed or timed out *"


if diff output expected; then
	echo "### everything PASS ###"
	clean_up
	exit 0
else
	echo "### something FAIL ###"
	#clean_up
	exit 1
fi
