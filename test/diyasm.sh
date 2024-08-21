#!/bin/bash
set -e

KITTENS_DIR=$(dirname $(dirname "$0"))
INPUT="$1"
if [ -z "$INPUT" ]; then
    INPUT="$KITTENS_DIR/test/kittens.dat"
fi

PREFIX="test"
FLAGS="-model aarch64.cat"
while IFS= read -r kitten; do
    if [ -z "$kitten" ]; then
        continue
    fi
    echo "[TEST] $kitten"
    (
        cd $KITTENS_DIR
        diyone7 -arch C $kitten > $PREFIX.litc
	cmd/nope $PREFIX.litc > $PREFIX.litmus
        herd7 $FLAGS $PREFIX.litmus > $PREFIX.log
        echo -n "       "
        if ! grep Sometimes $PREFIX.log; then
           cat $PREFIX.litc
           echo "-----------------------------------------------------"
           cat $PREFIX.litmus
           echo "-----------------------------------------------------"
           cat $PREFIX.log
           exit 1
       fi
    )
done < "$INPUT"

