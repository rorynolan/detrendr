
# `detrendr` <img src="man/figures/logo.png" align="right" height=141/>

Detrend fluorescence microscopy image series for fluorescence
fluctuation and correlation spectroscopy (FCS and FFS) analysis. This
package contains functionality published in a 2016 paper
<https://doi.org/10.1093/bioinformatics/btx434> but it has been extended
since then with the *Robin Hood* algorithm and thus contains unpublished
work.

[![R-CMD-check](https://github.com/rorynolan/detrendr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rorynolan/detrendr/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/github/rorynolan/detrendr/graph/badge.svg?token=GkXMOLXlAH)](https://codecov.io/github/rorynolan/detrendr)

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/detrendr)](https://cran.r-project.org/package=detrendr)
![RStudio CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/detrendr)
![RStudio CRAN monthly
downloads](https://cranlogs.r-pkg.org/badges/detrendr)

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)

## Installation

You can install the release version of `detrendr` from
[CRAN](https://CRAN.R-project.org/package=detrendr) with:

``` r
install.packages("detrendr")
```

You can install the (unstable) development version of `detrendr` from
[GitHub](https://github.com/rorynolan/detrendr/) with:

``` r
devtools::install_github("rorynolan/detrendr")
```

I highly recommend using the release version. The dev version is just
for the ultra-curious and should be thought of as unreliable.

## How to use the package

See the package website at <https://rorynolan.github.io/detrendr/>.

## Contribution

Contributions to this package are welcome. The preferred method of
contribution is through a github pull request. Feel free to contact me
by creating an issue. Please note that this project is released with a
[Contributor Code of
Conduct](https://github.com/rorynolan/detrendr/blob/master/CONDUCT.md).
By participating in this project you agree to abide by its terms.
