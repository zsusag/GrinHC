#!/bin/bash
COMMAND="stack"
OPTIONS=" exec GrinHC -- "
LEX_COMMAND="--lex "
PARSE_COMMAND="--parse "

TEST_SUFFIX="/*.in"
EVAL_SUFFIX=".out"
LEX_SUFFIX=".out.lex"
PARSE_SUFFIX=".out.parse"

#EVALFILES=$TEST_DIR$EVAL_SUFFIX
#LEXFILES=$TEST_DIR$LEX_SUFFIX
#PARSEFILES=$TEST_DIR$PARSE_SUFFIX

if [ -z "$1" ]
then
    echo "Usage: generate_baseline.sh <test|testdir>"
    exit 1
fi

TEST_DIR="$(realpath $1)"

if [ -d $TEST_DIR ] && [ "$(basename $TEST_DIR)" == "baselines" ]
then
    INFILES=$TEST_DIR$TEST_SUFFIX
    for file in $INFILES; do
        echo -n "Generating baseline for $file"

        # Construct names for outfiles
        eval_outfile="${file%.*}"$EVAL_SUFFIX
        lex_outfile="${file%.*}"$LEX_SUFFIX
        parse_outfile="${file%.*}"$PARSE_SUFFIX

        # Generate outfiles
        echo -n "."
        eval $COMMAND$OPTIONS$file > $eval_outfile 2>&1
        echo -n "."
        eval $COMMAND$OPTIONS$LEX_COMMAND$file > $lex_outfile 2>&1
        echo -n "."
        eval $COMMAND$OPTIONS$PARSE_COMMAND$file > $parse_outfile 2>&1
        
        echo "Done"
    done
fi
