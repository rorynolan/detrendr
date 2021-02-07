

## Test environments

* local mac OS X install, R 4.0.3
* ubuntu 14.04 (on travis-ci), R 4.0.3
* Windows Server 2012 (on AppVeyor), R 4.0.3
* win-builder (devel and release)


# R CMD check results

0 errors | 0 warnings | 2 notes

* Note 1. GNU make is a system requirement. This is needed to use RcppParallel.
* Note 2. Installed size is >5Mb due to libs directory. This is due to the package using a lot of Rcpp code.


## Reverse dependencies
* There is one reverse dependency `nandb` which has no changes to the worse upon this update of `detrendr`.
