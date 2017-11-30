### 0.2.0

#### NEW FEATURES
* The process of extending time series prior to smoothing is not done any more. This was introducing errors for images with low counts. Smoothing works fine without it. This gives a massive improvement in detrending speed.
* TIFF I/O and image display are now taken care of by the `ijtiff` package.
* Images are now represented in the style of an `ijtiff::ijtiff_img`.

#### DEFUNCT
* This package no longer exports functions for TIFF I/O nor image display.


# detrendr 0.1.0
* The first CRAN-worthy version.



