#!/bin/bash -xue

pushd reedsolomon/cbits
autoreconf -fvi
popd

stack \
    --no-terminal \
    setup \
    --resolver=$RESOLVER

stack \
    --no-terminal \
    install \
    --resolver=$RESOLVER \
    bytestring process process-extras QuickCheck
