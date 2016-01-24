#!/bin/bash -xue

pushd cbits
autoreconf -fvi
popd

stack \
    --no-terminal \
    install \
    bytestring process process-extras QuickCheck
