
<!-- index.md is generated from index.Rmd. Please edit that file -->

# `detrendr` <img src="man/figures/logo.png" align="right" height=140/>

*Detrending* is a technique to remove unwanted trends from time-series
data. Image series (videos) may be viewed as a collection of time
series: each pixel is its own time series, the value at time `t` being
the intensity value of that pixel in the frame recorded at time `t`.
Detrending is applied to image series in the fields of fluorescence
fluctuation and correlation spectroscopy (FCS and FFS) to remove trends
introduced by photobleaching and also other possible sources of trends
such as laser power fluctuation. `detrendr` is an R package for
detrending image series.

If you’re new to R and you’re here because you want to use `detrendr`,
be warned that you will need to learn some basic R first. I recommend
reading the short book “Hands On Programming with R” by Grolemund. This
is available for free at <https://rstudio-education.github.io/hopr/>.
That should be enough but if you want further reading, check out “R for
Data Science” which is available for free at <https://r4ds.had.co.nz/>.

This website gives an introduction to the `detrendr` package, assuming
that the reader has a basic level of R knowledge.

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

## Using `detrendr`

There are two ways to use `detrendr`.

1.  Interactively in the R session, playing with the image as a numeric
    array, dealing with one image at a time.
2.  In *batch* mode, having the software read TIFFs, perform the
    detrending and then write the detrended TIFFs to disk when
    detrending is over. This method permits the user to use R as little
    as possible and is better for those who don’t intend to become bona
    fide R users.

These are discussed in two articles.

1.  [Detrending single
    images](https://rorynolan.github.io/detrendr/articles/single-images.html)
2.  [Detrending many images in *batch*
    mode](https://rorynolan.github.io/detrendr/articles/batch-mode.html)

### Linescan data

The article [Linescan
data](https://rorynolan.github.io/detrendr/articles/linescan-data.html)
shows how to deal with data in linescan (as opposed to stack) format. If
you don’t know what linescan data is, you don’t need to read this
article.
