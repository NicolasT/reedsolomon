#!/bin/bash -xue

function build() {
    local suffix=$1
    shift
    local host=$1
    shift
    local cflags=$1
    shift
    local exec_suffix=$1
    shift
    local interpreter=$1

    set +e
    ./configure --host=${host} CFLAGS="${cflags}" INTERPRETER="${interpreter}"
    RC=$?
    set -e

    if ! test x$RC = x0; then
        test -f config.log && cat config.log
        return $RC
    fi

    make reedsolomon-gal-mul-stdio${exec_suffix} V=1
    make check

    mv reedsolomon-gal-mul-stdio${exec_suffix} reedsolomon-gal-mul-stdio-${suffix}${exec_suffix}

    make distclean
}

VALIDATE=$(dirname $0)/reedsolomon-gal-mul-stdio-quickcheck.hs
test -f ${VALIDATE}

unset CC
cd cbits

HOST_ISA=`uname -p`

build $HOST_ISA '' '' '' ''
build arm arm-linux-gnueabihf '' '' 'qemu-arm-static -L /usr/arm-linux-gnueabihf'
build arm-neon arm-linux-gnueabihf '-mfpu=neon' '' 'qemu-arm-static -L /usr/arm-linux-gnueabihf'
build ppc64le powerpc64le-linux-gnu '-mcpu=power6 -mno-altivec' '' 'qemu-ppc64le-static -cpu POWER8 -L /usr/powerpc64le-linux-gnu'
build ppc64le-altivec powerpc64le-linux-gnu '-mcpu=power6 -maltivec' '' 'qemu-ppc64le-static -cpu POWER8 -L /usr/powerpc64le-linux-gnu'
build ppc64le-power8 powerpc64le-linux-gnu '-mcpu=power8' '' 'qemu-ppc64le-static -cpu POWER8 -L /usr/powerpc64le-linux-gnu'
build aarch64 aarch64-linux-gnu '' '' 'qemu-aarch64-static -L /usr/aarch64-linux-gnu'
build x86_64-w64-mingw32 x86_64-w64-mingw32 '' '.exe' 'wine'
build i686-w64-mingw32 i686-w64-mingw32 '' '.exe' 'wine'

stack runhaskell --resolver=$RESOLVER ${VALIDATE} -- \
    ./reedsolomon-gal-mul-stdio-$HOST_ISA \
    'qemu-arm-static -L /usr/arm-linux-gnueabihf ./reedsolomon-gal-mul-stdio-arm'
stack runhaskell --resolver=$RESOLVER ${VALIDATE} -- \
    ./reedsolomon-gal-mul-stdio-$HOST_ISA \
    'qemu-arm-static -L /usr/arm-linux-gnueabihf ./reedsolomon-gal-mul-stdio-arm-neon'

stack runhaskell --resolver=$RESOLVER ${VALIDATE} -- \
    ./reedsolomon-gal-mul-stdio-$HOST_ISA \
    'qemu-ppc64le-static -cpu POWER8 -L /usr/powerpc64le-linux-gnu ./reedsolomon-gal-mul-stdio-ppc64le'
stack runhaskell --resolver=$RESOLVER ${VALIDATE} -- \
    ./reedsolomon-gal-mul-stdio-$HOST_ISA \
    'qemu-ppc64le-static -cpu POWER8 -L /usr/powerpc64le-linux-gnu ./reedsolomon-gal-mul-stdio-ppc64le-altivec'
stack runhaskell --resolver=$RESOLVER ${VALIDATE} -- \
    ./reedsolomon-gal-mul-stdio-$HOST_ISA \
    'qemu-ppc64le-static -cpu POWER8 -L /usr/powerpc64le-linux-gnu ./reedsolomon-gal-mul-stdio-ppc64le-power8'
stack runhaskell --resolver=$RESOLVER ${VALIDATE} -- \
    ./reedsolomon-gal-mul-stdio-$HOST_ISA \
    'qemu-aarch64-static -L /usr/aarch64-linux-gnu ./reedsolomon-gal-mul-stdio-aarch64'

stack runhaskell --resolver=$RESOLVER ${VALIDATE} -- \
    ./reedsolomon-gal-mul-stdio-$HOST_ISA \
    'wine ./reedsolomon-gal-mul-stdio-x86_64-w64-mingw32.exe'
stack runhaskell --resolver=$RESOLVER ${VALIDATE} -- \
    ./reedsolomon-gal-mul-stdio-$HOST_ISA \
    'wine ./reedsolomon-gal-mul-stdio-i686-w64-mingw32.exe'
