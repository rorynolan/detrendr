## Test environments
* local mac OS X install, R 3.4.4
* local Ubuntu 16.04 install with valgrind, R 3.4.4
* ubuntu 14.04 (on travis-ci), R 3.4.4
* Windows Server 2012 (on AppVeyor), R 3.4.4
* debian (on r-hub), R 3.4.4
* fedora (on r-hub), R 3.4.4
* linux (on r-hub), R 3.4.4
* macos (on r-hub), R 3.4.4
* ubuntu (on r-hub), R 3.4.4
* solaris (on r-hub), R 3.4.4
* win-builder (devel and release)

# R CMD check results

0 errors | 0 warnings | 1 note

* Note 1. GNU make is a system requirement. This is needed to use RcppParallel.

## Reverse dependencies
There are no reverse dependencies.

## CRAN test results
* Some of the current CRAN test results are ERRORs due to differences in numerical percision across platforms. This version addresses those ERRORs.
* Additional CRAN checks throw up issues for clang-UBSAN and gcc-UBSAN. These are from an issue with RcppParallel, which in turn is an issue from Intel's TBB library. These issues are harmless sanitizer things but will only be fixed when Intel irons them out. See discussion at https://github.com/RcppCore/RcppParallel/issues/36.
