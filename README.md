detrendr
================

Detrend image series.

[![Travis-CI Build
Status](https://travis-ci.org/rorynolan/detrendr.svg?branch=master)](https://travis-ci.org/rorynolan/detrendr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/rorynolan/detrendr?branch=master&svg=true)](https://ci.appveyor.com/project/rorynolan/detrendr)
[![Coverage
Status](https://img.shields.io/codecov/c/github/rorynolan/detrendr/master.svg)](https://codecov.io/github/rorynolan/detrendr?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/detrendr)](https://cran.r-project.org/package=detrendr)
![RStudio CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/detrendr)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

# Installation

To install the release version (recommended) from CRAN, in R, enter

``` r
install.packages("detrendr")
```

To install the development version, in R, first install `devtools` via
`install.packages("devtools")`. Then enter

``` r
devtools::install_github("rorynolan/detrendr")
```

# Use

First let’s load the library:

``` r
library(detrendr)
```

## An image in need of detrending

The package contains a sample image series which can be found at  
`system.file("extdata", "bleached.tif", package = "detrendr")`. It’s 500
frames of diffusing fluorescent particles which are bleaching over the
course of the acquisition. We can see they’re bleaching by displaying
every 99th frame.

``` r
library(magrittr)
path <- system.file("extdata", "bleached.tif", package = "detrendr")
img <- ijtiff::read_tif(path, msg = FALSE)
every100th <- purrr::map(seq(1, dim(img)[4], by = 99), ~ img[, , 1, .]) %>% 
  purrr::reduce(~ cbind(.x, max(img), .y))
ijtiff::display(every100th)
```

![](README_files/figure-gfm/visualize%20bleaching-1.png)<!-- -->

## Detrending

We see that the intensity is much lower for the last frame, this is
because the image series has been bleached. We can correct for this.

``` r
system.time(corrected_exp <- img_detrend_exp(img, "auto", 
                                             seed = 0, parallel = 2))["elapsed"]
```

    #> elapsed 
    #>   2.515

``` r
every100th <- purrr::map(seq(1, dim(img)[4], by = 99), 
                         ~ corrected_exp[, , 1, .]) %>% 
  purrr::reduce(~ cbind(.x, max(img), .y))
ijtiff::display(every100th)
```

![](README_files/figure-gfm/detrend-1.png)<!-- -->

So we see that the corrected series does not have this drop-off in
intensity.

# Vignette

For more detailed instruction on how to use the package, see
`vignette("detrendr")`.

# Contribution

Contributions to this package are welcome. The preferred method of
contribution is through a github pull request. Feel free to contact me
by creating an issue. Please note that this project is released with a
[Contributor Code of Conduct](CONDUCT.md). By participating in this
project you agree to abide by its terms.
