#!/bin/bash -ue

HOST_ISA=`uname -p`

function build_for_arch() {
    local isa=$1
    shift
    local backend=$1
    shift
    local cflags=${1:-}

    PATH=~/x-tools/$isa/bin:$PATH ./configure --host=$isa CFLAGS="$cflags"
    PATH=~/x-tools/$isa/bin:$PATH make reedsolomon-gal-mul-stdio
    mv reedsolomon-gal-mul-stdio reedsolomon-gal-mul-stdio-$isa-$backend
    make clean
}

function test_arch() {
    local qemu_arch=$1
    shift
    local isa=$1
    shift
    local backend=$1

    echo "Validating $isa with backend $backend"
    stack runhaskell reedsolomon-gal-mul-stdio-quickcheck.hs -- \
        ./reedsolomon-gal-mul-stdio-$HOST_ISA-native \
        "qemu-$qemu_arch -L ~/x-tools/$isa/$isa/sysroot ./reedsolomon-gal-mul-stdio-$isa-$backend"
    echo
}

build_for_arch $HOST_ISA native
build_for_arch armv7-rpi2-linux-gnueabihf generic '-mfpu=vfp'
build_for_arch armv7-rpi2-linux-gnueabihf neon '-mfpu=neon'
# My PPC64 x-tools require `-static` for executables to work with `qemu-ppc64`
build_for_arch powerpc64-unknown-linux-gnu generic '-static'
build_for_arch powerpc64-unknown-linux-gnu altivec '-static -maltivec'

test_arch arm armv7-rpi2-linux-gnueabihf generic
test_arch arm armv7-rpi2-linux-gnueabihf neon
test_arch ppc64 powerpc64-unknown-linux-gnu generic
test_arch ppc64 powerpc64-unknown-linux-gnu altivec
