

## Test environments

* local mac OS X install, R 3.5.3
* ubuntu 14.04 (on travis-ci), R 3.5.3
* Windows Server 2012 (on AppVeyor), R 3.5.3
* win-builder (devel and release)


# R CMD check results

0 errors | 0 warnings | 1 note

* Note 1. GNU make is a system requirement. This is needed to use RcppParallel.


## Reverse dependencies
* There is one reverse dependency `nandb` which has no changes to the worse upon this update of `detrendr`.

## CRAN test results
* Additional CRAN checks throw up issues for clang-UBSAN and gcc-UBSAN. These are from an issue with RcppParallel, which in turn is an issue from Intel's TBB library. These issues are harmless sanitizer things but will only be fixed when Intel irons them out. See discussion at https://github.com/RcppCore/RcppParallel/issues/36.
