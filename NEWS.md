### 0.5.1

#### BUG FIXES
* Fix tests for CRAN fedora and mac.


### 0.5.0

#### NEW FEATURES
* The package no longer depends on `RSAGA`, making it lighter.
* _Robin Hood_ detrending has been added.


### 0.4.0

#### NEW FEATURES
* Add the option to detrend for the purpose of FCS or FFS.


### 0.3.0

#### NEW FEATURES
* Batch processing: detrend an entire folder with the likes of `dir_detrend_exp()`.

#### BUG FIXES
* Asymmetric images caused R to crash.
* `NA`s in simulated brightnesses were needlessly causing the automatic parameter-finding routines to fail.


### 0.2.0

#### NEW FEATURES
* The process of extending time series prior to smoothing is not done any more. This was introducing errors for images with low counts. Smoothing works fine without it. Dropping this extension gives a massive improvement in detrending speed :-)
* TIFF I/O and image display are now taken care of by the `ijtiff` package.
* Images are now represented in the style of an `ijtiff::ijtiff_img`.

#### DEFUNCT
* This package no longer exports functions for TIFF I/O nor image display.


### detrendr 0.1.0
* The first CRAN-worthy version.
