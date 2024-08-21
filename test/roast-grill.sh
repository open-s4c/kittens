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
        cmd/grill.scm $kitten | z3 -in | cmd/roast.scm - > $PREFIX.c
        herd7 $FLAGS $PREFIX.c > $PREFIX.log
        echo -n "       "
        if ! grep Sometimes $PREFIX.log; then
           cat $PREFIX.c
           echo "-----------------------------------------------------"
           cat $PREFIX.log
           exit 1
       fi
    )
done < "$INPUT"

