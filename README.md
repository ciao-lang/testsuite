# A general test suite for Ciao

This is a general test suite for the Ciao language, its
implementation, and the integration of several of its tools and
components (that is, integration tests and benchmarks).

Tests here should allow measuring:

 - correctness of compiler, optimizer, analyzer, etc. (including
   parser, pretty print, output, ...)

 - performance (time, memory consumption) of the engine, runtime,
   libraries, compiler and analysis

 - comparison with other languages and Prolog implementations

**NOTE**: Do not include unit tests for particular libraries here.

**NOTE**: See the `iso_tests` bundle for a ISO standard testsuite

## Usage

Clone this repository into your workspace or install with `ciao get
testsuite`. Then run:
```sh
ciao test testsuite # run tests
ciao bench testsuite # run benchmarks
```
The `testsuite` bundle name is optional if you execute `ciao` from the
`testsuite` directory.

