#!/bin/bash
COMMAND="stack"
OPTIONS=" exec GrinHC -- "

TEST_SUFFIX=".lsp"
OUT_SUFFIX=".out"

if [ -z "$1" ]
then
    echo "Usage: check_baseline.sh <test|testdir>"
    exit 1
fi

TEST_DIR="$(realpath $1)"
FAILED=false

if [ -d $TEST_DIR ] && [ "$(basename $TEST_DIR)" == "assignment_02" ]
then
    for file in $TEST_DIR/*.lsp; do
        echo "Checking $file..."
        # Create a tmp file within /tmp
        tmpfile=$(mktemp)
        
        # Evaluate and diff the output
        eval $COMMAND$OPTIONS$file > $tmpfile
        outfile="${file%.*}.out"
        diff_output="$(diff $tmpfile $outfile)"
        if [ -n "$diff_output" ]
        then
            echo "$diff_output"
            FAILED=true
        fi
        rm $tmpfile
    done
    echo "Done testing in $TEST_DIR."
fi

# Exit with failure since a test failed
if [ $FAILED = true ]
then
    exit 1
fi

# Exit successfully, indicating tests passed
exit 0
