## Test environments
- local OS X install, R 4.2.1
- ubuntu 20.04 (on GitHub Actions), R 4.2.1
- Windows server 2022 (on GitHub Actions), R 4.2.1
- win-builder (devel and release)
- rhub::check_for_cran()


# R CMD check results

0 errors | 0 warnings | 2 notes

* Note 1. GNU make is a system requirement. This is needed to use RcppParallel.
* Note 2. Installed size is >5Mb due to libs directory. This is due to the package using a lot of Rcpp code.


## Reverse dependencies
* There is one reverse dependency `nandb` which has no changes to the worse upon this update of `detrendr`.
