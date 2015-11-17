# Reed-Solomon
[![Build Status][3]][4]

[3]: https://travis-ci.org/NicolasT/reedsolomon.svg?branch=master
[4]: https://travis-ci.org/NicolasT/reedsolomon

Reed-Solomon Erasure Coding in Haskell, with speeds exceeding 1GB/s/cpu core implemented in pure Haskell (and some SIMD C/assembler).

This is a Haskell port of the [GolangReedSolomon](https://github.com/klauspost/reedsolomon) library released by [Klaus Post](http://klauspost.com/), wich is a port of the [JavaReedSolomon](https://github.com/Backblaze/JavaReedSolomon) library released by [Backblaze](http://backblaze.com), with some additional optimizations.

For an introduction on erasure coding, see the post on the [Backblaze blog](https://www.backblaze.com/blog/reed-solomon/).

Package home: https://github.com/NicolasT/reedsolomon

# Links
* [Backblaze Open Sources Reed-Solomon Erasure Coding Source Code](https://www.backblaze.com/blog/reed-solomon/).
* [GolangReedSolomon](https://github.com/klauspost/reedsolomon). Compatible Go library by Klaus Post.
* [JavaReedSolomon](https://github.com/Backblaze/JavaReedSolomon). Compatible java library by Backblaze.
* [go-erasure](https://github.com/somethingnew2-0/go-erasure). A similar library using cgo, slower in my tests.
* [Screaming Fast Galois Field Arithmetic](http://www.snia.org/sites/default/files2/SDC2013/presentations/NewThinking/EthanMiller_Screaming_Fast_Galois_Field%20Arithmetic_SIMD%20Instructions.pdf). Basis for SSE3 optimizations.

# License

This code, as the original [GolangReedSolomon](https://github.com/klauspost/reedsolomon) and [JavaReedSolomon](https://github.com/Backblaze/JavaReedSolomon) is published under an MIT license. See LICENSE file for more information.
