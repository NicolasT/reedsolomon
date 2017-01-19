#!/bin/bash
set -u
set -e
set -o pipefail

INPUT=$1
SIZE=$(( ($(wc -c "${INPUT}" | awk '{ print $1 }') - 16 - 16) / 2 ))
REFERENCE_OFFSET=$(( SIZE + 16 + 16 ))

function do_test() {
        local cnt=$1
        shift
        local kind=$1
        shift
        local lower_kind
        lower_kind=$( echo "${kind}" | tr '[:upper:]' '[:lower:]' )
        local output
        output=$( mktemp )

        echo "# Settings:"
        echo "#     kind=${kind}"
        echo "#     lower_kind=${lower_kind}"
        echo "#     INPUT=${INPUT}"
        echo "#     SIZE=${SIZE}"
        echo "#     REFERENCE_OFFSET=${REFERENCE_OFFSET}"
        echo "#     output=${output}"
        echo "#     INTERPRETER=${INTERPRETER:-}"

        set +e
        ${INTERPRETER:-} ./reedsolomon-gal-mul-stdio -b "${lower_kind}" "${SIZE}" < "${INPUT}" > "${output}"
        local RC=$?
        set -e

        case "${RC}" in
                "0")
                        set +e
                        cmp -i "${REFERENCE_OFFSET}:0" "${INPUT}" "${output}"
                        local RC=$?
                        set -e
                        rm -f "${output}"
                        case "${RC}" in
                                "0")
                                        echo "ok ${cnt} - ${kind}";;
                                *)
                                        echo "not ok ${cnt} - ${kind} (cmp failure, rc=${RC})";;
                        esac;;
                "77")
                        rm -f "${output}"
                        echo "ok ${cnt} - ${kind} # SKIP Instructions not supported";;
                "99")
                        rm -f "${output}"
                        echo "Bail out! Failure during reedsolomon-gal-mul-stdio execution";;
                *)
                        rm -f "${output}"
                        echo "not ok ${cnt} - ${kind} (reedsolomon-gal-mul-stdio failure, rc=${RC})";;
        esac
}

echo 1..8
echo "# Calculated size for test ${INPUT}: ${SIZE}"
do_test 1 Native
do_test 2 Generic
do_test 3 SSE2
do_test 4 SSSE3
do_test 5 AVX
do_test 6 AVX2
do_test 7 Neon
do_test 8 AltiVec
