#!/bin/bash -ue

pushd cbits
autoreconf -fvi
popd

stack \
    --no-terminal \
    setup \
    --resolver=$RESOLVER

stack \
    --no-terminal \
    build \
    ${STACK_BUILD_OPTIONS[*]:-} \
    --test \
    --bench \
    --only-snapshot \
    --resolver=$RESOLVER
