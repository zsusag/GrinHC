#!/bin/bash
COMMAND="stack"
OPTIONS=" exec GrinHC -- "
LEX_COMMAND="--lex "
PARSE_COMMAND="--parse "

TEST_SUFFIX=".in"
EVAL_SUFFIX=".out"
LEX_SUFFIX=".out.lex"
PARSE_SUFFIX=".out.parse"


if [ -z "$1" ]
then
    echo "Usage: check_baseline.sh <test|testdir>"
    exit 1
fi

TEST_DIR="$(realpath $1)"
FAILED=false

if [ -d $TEST_DIR ] && [ "$(basename $TEST_DIR)" == "baselines" ]
then
    for file in "$TEST_DIR"/*"$TEST_SUFFIX"; do
        echo -n "Checking $file"
        # Create temp files within /tmp
        tmpfile_eval=$(mktemp)
        tmpfile_lex=$(mktemp)
        tmpfile_parse=$(mktemp)

        # Create outfile name for tested file
        eval_outfile="${file%.*}"$EVAL_SUFFIX
        lex_outfile="${file%.*}"$LEX_SUFFIX
        parse_outfile="${file%.*}"$PARSE_SUFFIX

        echo -n "."

        # Evaluate and diff the output
        eval $COMMAND$OPTIONS$file > $tmpfile_eval
        eval $COMMAND$OPTIONS$LEX_COMMAND$file > $tmpfile_lex
        eval $COMMAND$OPTIONS$PARSE_COMMAND$file > $tmpfile_parse

        # Generate diffs
        diff_output_eval="$(diff $tmpfile_eval $eval_outfile)"
        diff_output_lex="$(diff $tmpfile_lex $lex_outfile)"
        diff_output_parse="$(diff $tmpfile_parse $parse_outfile)"

        echo -n "."

        # Print out any differences between generated output and outfiles
        if [ -n "$diff_output_eval" ]
        then
            echo "$diff_output_eval"
            FAILED=true
        fi
        if [ -n "$diff_output_lex" ]
        then
            echo "$diff_output_lex"
            FAILED=true
        fi
        if [ -n "$diff_output_parse" ]
        then
            echo "$diff_output_parse"
            FAILED=true
        fi

        echo -n "."
        rm $tmpfile_eval
        rm $tmpfile_lex
        rm $tmpfile_parse

        echo "Done"
    done
    echo "Completed Testing."
fi

# Exit with failure since a test failed
if [ $FAILED = true ]
then
    exit 1
fi

# Exit successfully, indicating tests passed
exit 0
