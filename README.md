detrendr
================

Detrend image series.

[![Travis-CI Build Status](https://travis-ci.org/rorynolan/detrendr.svg?branch=master)](https://travis-ci.org/rorynolan/detrendr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rorynolan/detrendr?branch=master&svg=true)](https://ci.appveyor.com/project/rorynolan/detrendr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/detrendr)](https://cran.r-project.org/package=detrendr) ![RStudio CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/detrendr) [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

Installation
============

To install the release version (recommended) from CRAN, in R, enter

``` r
install.packages("detrendr")
```

To install the development version, in R, first install `devtools` via `install.packages("devtools")`. Then enter

``` r
devtools::install_github("rorynolan/detrendr")
```

Use
===

First let's load the library:

``` r
library(detrendr)
```

Image I/O and display
---------------------

The package contains a sample image series which can be found at `system.file("extdata", "cells.tif", package = "detrendr")`. Let's read it in and inspect the first and last frames:

``` r
path <- system.file("extdata", "bleached.tif", package = "detrendr")
img <- ijtiff::read_tif(path)
dim(img)  # img has 500 frames
```

    #> [1]  60  60   1 500

``` r
mean(img[, , 1, 1])  # first channel, first frame
```

    #> [1] 152.4489

``` r
mean(img[, , 1, 500])  # first channel, last frame
```

    #> [1] 68.51583

Detrending
----------

We see that the intensity is much lower for the last frame, this is because the image series has been bleached. We can correct for this (and check how long it takes):

``` r
system.time(corrected <- img_detrend_exp(img, "auto", 
                                         seed = 0, parallel = 2))["elapsed"]
```

    #> elapsed 
    #>   7.589

``` r
mean(corrected[, , 1, 1])  # first channel, first frame
```

    #> [1] 112.9569

``` r
mean(corrected[, , 1, 500])  # first channel, last frame
```

    #> [1] 103.33

So we see that the corrected series does not have this drop-off in intensity.

Vignette
========

For more detailed instruction on how to use the package, see `vignette("detrendr")`.

Contribution
============

Contributions to this package are welcome. The preferred method of contribution is through a github pull request. Feel free to contact me by creating an issue. Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
