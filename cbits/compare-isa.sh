#!/bin/bash -xue

HOST_ISA=x86_64
OTHER_ISA=armv7-rpi2-linux-gnueabihf
QEMU_ISA=arm

test -f Makefile && make distclean
./configure
make reedsolomon-gal-mul-stdio
mv reedsolomon-gal-mul-stdio reedsolomon-gal-mul-stdio-$HOST_ISA

make distclean
PATH=~/x-tools/$OTHER_ISA/bin:$PATH ./configure --host=$OTHER_ISA
PATH=~/x-tools/$OTHER_ISA/bin:$PATH make reedsolomon-gal-mul-stdio
mv reedsolomon-gal-mul-stdio reedsolomon-gal-mul-stdio-$OTHER_ISA

stack runhaskell reedsolomon-gal-mul-stdio-quickcheck.hs -- \
        -v \
        ./reedsolomon-gal-mul-stdio-$HOST_ISA \
        "qemu-$QEMU_ISA -L ~/x-tools/$OTHER_ISA/$OTHER_ISA/sysroot ./reedsolomon-gal-mul-stdio-$OTHER_ISA"
