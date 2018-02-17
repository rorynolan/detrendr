## Test environments
* local OS X install, R 3.4.3
* ubuntu 14.04 (on travis-ci), R 3.4.3
* ubuntu 16.04 with valgrind, R 3.4.3
* Windows Server 2012 (on AppVeyor), R 3.4.3
* win-builder (devel and release)

# R CMD check results

0 errors | 0 warnings | 1 note

Note 1. GNU make is a system requirement. This is needed to use RcppParallel.

## Reverse dependencies
There are no reverse dependencies.

## CRAN test results
* Some of the current CRAN test results are ERRORs due to differences in numerical percision across platforms. This version addresses those by including higher tolerance.
* Some of the current CRAN test results are NOTEs because the package is large. This version brings the package size under 5MB by reducing the size of the example image.
* Additional CRAN checks throw up issues for clang-UBSAN and gcc-UBSAN. These are from an issue with RcppParallel, which in turn is an issue from Intel's TBB library. These issues are harmless sanitizer things but will only be fixed when Intel irons them out. See discussion at https://github.com/RcppCore/RcppParallel/issues/36.
