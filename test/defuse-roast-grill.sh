#!/bin/bash
set -e

KITTENS_DIR=$(dirname $(dirname "$0"))
INPUT="$1"
if [ -z "$INPUT" ]; then
    INPUT="$KITTENS_DIR/test/kittens.dat"
fi

PREFIX="test"
LITC_FLAGS="-c11 -cat fences.cat"
HERD7_FLAGS="-cat aarch64.cat"
DAT3M_FLAGS="-bound 5 -cat models/aarch64.cat"
while read -r kitten; do
    if [ -z "$kitten" ]; then
        continue
    fi
    echo "[TEST] $kitten"

    if echo "$kitten" | grep RMW > /dev/null; then
        echo -n "       "
        echo skip
        continue
    fi

    (
        cd $KITTENS_DIR
        cmd/grill.scm $kitten > $PREFIX.smt
        if ! z3 $PREFIX.smt > $PREFIX.model; then
            echo -n "       "
            echo unsat
        else
                cmd/roast.scm $PREFIX.model > $PREFIX.litc
                herd7 $LITC_FLAGS $PREFIX.litc > $PREFIX.litc.log
                echo -n "       "
                echo -n "herd7/litc:   "
                if ! grep Sometimes $PREFIX.litc.log; then
                   cat $PREFIX.litc
                   echo "-----------------------------------------------------"
                   cat $PREFIX.litc.log
                   echo "-----------------------------------------------------"
                   exit 1
                fi

           	cmd/nope $PREFIX.litc > $PREFIX.litmus


                herd7 $HERD7_FLAGS $PREFIX.litmus > $PREFIX.litmus.log
                echo -n "       "
                echo -n "herd7/litmus: "
                if grep Sometimes $PREFIX.litmus.log; then
                   echo "unexpected failure"
                   cat $PREFIX.litc
                   echo "-----------------------------------------------------"
                   cat $PREFIX.litmus
                   echo "-----------------------------------------------------"
                   cat $PREFIX.litmus.log
                   exit 1
                else
                    echo OK
                fi



                dat3m $DAT3M_FLAGS $PREFIX.litmus > $PREFIX.litmus.log
                echo -n "       "
                echo -n "dat3m/litmus: "
                if grep UNKNOWN $PREFIX.litmus.log > /dev/null; then
			echo UNKNOWN
			exit 1
		elif grep FAIL $PREFIX.litmus.log > /dev/null; then
			echo OK
                else
                   	echo "Found witness (bad!)"
                   	exit 1
                fi

       fi
    )
done < "$INPUT"

