#!/bin/bash
set -e

KITTENS_DIR=$(dirname $(dirname "$0"))
PREFIX="test"
LITC_FLAGS="-c11 -cat fences.cat"
HERD7_FLAGS="-cat aarch64.cat"
DAT3M_FLAGS="-bound 5 -cat models/aarch64.cat"

HERD7_ENABLED=
DAT3M_ENABLED=
RMW_ENABLED=
VATOMIC_ENABLED=

INPUT="$1"
if [ -z "$INPUT" ]; then
    INPUT="$KITTENS_DIR/test/kittens.dat"
fi

if [ "$INPUT" = "-in" ]; then
    INPUT=/dev/stdin
fi

VATOMIC_ENABLED=true


function echopfx {
    echo -n "      "
}

function echop {
    echopfx
    echo $*
}

function grill {
    kitten="$1"

    if ! cmd/grill.scm $kitten > $PREFIX.smt; then
        echop grill failed
        return 1
    fi

    if ! z3 $PREFIX.smt > $PREFIX.model; then
        echop unsat
        return 1
    fi

    echop grilled
}

function roast {
    if ! cmd/roast.scm $PREFIX.model > $PREFIX.litc; then
        echop roast failed
        return 1
    fi

    herd7 $LITC_FLAGS $PREFIX.litc > $PREFIX.litc.log
    if ! grep Sometimes $PREFIX.litc.log > /dev/null; then
       echop herd7 failed
       echo "-----------------------------------------------------"
       cat $PREFIX.litc
       echo "-----------------------------------------------------"
       cat $PREFIX.litc.log
       echo "-----------------------------------------------------"
       return 1
    fi

    echop roasted
}

function vatomic_check {
    if [ "$VATOMIC_ENABLED" = "true" ]; then
        if [ ! -f include/stdatomic.h ]; then
            cp include/stdatomic.bak.h include/stdatomic.h
        fi
    else
        rm -f include/stdatomic.h
    fi
}

function defuse {
    if ! cmd/nope $PREFIX.litc > $PREFIX.litmus 2> $PREFIX.err; then
        echop "defuse failed"
        echo
        cat $PREFIX.err
        cat $PREFIX.litc
        return 1
    fi

    echop defused
}

function drop  {
    kitten="$1"
    if [ "$RMW_ENABLED" != "true" ]; then
        if echo "$kitten" | grep RMW > /dev/null; then
            echop skip RMW
            return 0
        fi
    fi
    return 1
}

function herd7-check {
    if [ "$HERD7_ENABLED" != "true" ]; then
        return 0
    fi

    herd7 $HERD7_FLAGS $PREFIX.litmus > $PREFIX.litmus.log
    if grep Sometimes $PREFIX.litmus.log; then
       echop "unexpected failure"
       cat $PREFIX.litc
       echo "-----------------------------------------------------"
       cat $PREFIX.litmus
       echo "-----------------------------------------------------"
       cat $PREFIX.litmus.log
       return 1
    fi
    echop herd7: OK
}

function dat3m-check {
    if [ "$DAT3M_ENABLED" != "true" ]; then
        return 0
    fi

    dat3m $DAT3M_FLAGS $PREFIX.litmus > $PREFIX.litmus.log
    if grep UNKNOWN $PREFIX.litmus.log > /dev/null; then
        echop dat3m: UNKNOWN
    elif grep FAIL $PREFIX.litmus.log > /dev/null; then
        echop dat3m: OK
    else
        echop dat3m: FAILED
        return 1
    fi
}

(
    cd $KITTENS_DIR
    vatomic_check

    while read -r kitten; do
        if [ -z "$kitten" ]; then
            continue
        fi

        if echo "$kitten" | grep -e "^#" > /dev/null; then
            continue
        fi

        # valid kitten, start loop
        echo
        echo "TEST: $kitten"

        if drop "$kitten"; then
            continue
        fi

        if ! grill "$kitten"; then
            continue
        fi

        if ! roast; then
            continue
        fi

        if ! defuse; then
            exit 1
        fi

        if ! herd7-check; then
            exit 1
        fi

        if ! dat3m-check; then
            exit 1
        fi

    done < "$INPUT"
    echo END
)

