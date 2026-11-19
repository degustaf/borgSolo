#! /usr/bin/bash -eux

function opt_build {
	elm make src/Main.elm --output elm.js --optimize
	uglifyjs elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' --output elm.js
	uglifyjs elm.js --mangle --output elm.js
}

function debug_build {
	elm make src/Main.elm --output elm.js
}

function copy_artifacts {
	local artifact_dir=$1
	mkdir -p "$artifact_dir"
	cp --force ./index.html "$artifact_dir"
	cp --force ./elm.js "$artifact_dir"
	cp --force --recursive assets "$artifact_dir"
}

if [ $# -eq 0 ] ; then
	opt_build
elif [ "$1" == "optimize" ] ; then
	opt_build
elif [ "$1" == "debug" ] ; then
	debug_build
fi

if [ $# -gt 1 ] ; then
	copy_artifacts "$2"
fi
