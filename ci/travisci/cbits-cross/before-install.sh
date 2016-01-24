#!/bin/bash -xue

mkdir -p $LOCAL_BIN

curl -L https://www.stackage.org/stack/$TRAVIS_OS_NAME-x86_64 | \
    tar xz --wildcards --strip-components=1 -C $LOCAL_BIN '*/stack'
