#!/bin/bash
set -e

KITTENS_DIR=$(dirname $(dirname "$0"))
INPUT="$1"
if [ -z "$INPUT" ]; then
    INPUT="$KITTENS_DIR/test/kittens.dat"
fi

PREFIX="test"
FLAGS="-c11 -model fences.cat"
while read -r kitten; do
    if [ -z "$kitten" ]; then
        continue
    fi
    echo "[TEST] $kitten"
    (
        cd $KITTENS_DIR
        cmd/grill.scm $kitten > $PREFIX.smt
        if z3 $PREFIX.smt > $PREFIX.model; then
            cmd/roast.scm $PREFIX.model > $PREFIX.litc
            herd7 $FLAGS $PREFIX.litc > $PREFIX.litc.log
            echo -n "       "
            if ! grep Sometimes $PREFIX.litc.log; then
               cat $PREFIX.litc
               echo "-----------------------------------------------------"
               cat $PREFIX.litc.log
               exit 1
           fi
        fi
    )
done < "$INPUT"

