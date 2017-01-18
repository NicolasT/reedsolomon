#!/bin/bash
set -u
set -e
set -o pipefail

INPUT=$1
SIZE=$(( ($(wc -c "${INPUT}" | awk '{ print $1 }') - 16 - 16) / 2 ))
REFERENCE_OFFSET=$(( SIZE + 16 + 16 ))

./reedsolomon-gal-mul-stdio ${SIZE} < "${INPUT}" | cmp -i ${REFERENCE_OFFSET}:0 "${INPUT}"
