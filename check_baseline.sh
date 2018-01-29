#!/bin/bash

if [ -z "$1" ]
then
    echo "Usage: check_baseline.sh <test|testdir>"
    exit 1
fi

TEST_DIR="$(realpath $1)"

if [ -d $TEST_DIR ] && [ "$(basename $TEST_DIR)" == "assignment_01" ]
then
    for file in $TEST_DIR/*.test; do
        echo "Checking $file..."
        command="$(cat $file)"
        tmpfile="$file.tmp"
        eval $command > $tmpfile
        outfile="${file%.*}.out"
        diff $outfile $tmpfile
        rm $tmpfile
    done
    echo "Done testing in $TEST_DIR."
fi
