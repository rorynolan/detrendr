# `detrendr` 0.6.9

## BUG FIXES
* Fix for magrittr 2.0 (which no longer allows you to call `return(.)` on a magrittr dot).


# `detrendr` 0.6.8

## BUG FIXES
* An image needed for the tests is no longer included in `ijtiff` so `detrendr` needs to have it itself.


# `detrendr` 0.6.7

## BUG FIXES
* Skip a few tests on CRAN (necessary because CRAN has so many different machines with various RNG implementations).


# `detrendr` 0.6.6

## BUG FIXES
* Cope with `ijtiff` not working on 32-bit Windows.


# `detrendr` 0.6.5

## BUG FIXES
* `ggplot2` needed to be in `Suggests` because it is used in a vignette.


# `detrendr` 0.6.4

## BUG FIXES
* Insist on latest, bug-fixed `ijtiff` v2.0.2.


# `detrendr` 0.6.3

## BUG FIXES
* Ignore a test on CRAN mac (it's fine on travis mac).
* Insist on latest, bug-fixed `filesstrings` v3.1.5.


# `detrendr` 0.6.2

## BUG FIXES
* Demand latest, least buggy `filesstrings`, `ijtiff` and `autothresholdr`.
* Permanently fix the issues that were temporarily fixed in the previous version (R 3.6 has now landed).


# `detrendr` 0.6.1

## BUG FIXES
* Require necessary version of `glue`.
* Temporary fix for upcoming R 3.6 which patches `base::sample()`. A more permanent fix should be provided when R 3.6 has landed.


# `detrendr` 0.6.0 

## NEW FEATURES 
* A `pkgdown` website!

## MINOR IMPROVEMENTS
* Robin Hood parameter finding is now repeated several (at least 9) times to find a sensible consensus value.
* Robin Hood parameter finding includes an adjustment step to avoid over-estimating the number of swaps required.


# `detrendr` 0.5.2

## BUG FIXES
* Detrending was not working well for images which had dimension 1 in x or y.


# `detrendr` 0.5.1

## BUG FIXES
* Fix tests for CRAN fedora and mac.


# `detrendr` 0.5.0

## NEW FEATURES
* The package no longer depends on `RSAGA`, making it lighter.
* _Robin Hood_ detrending has been added.


# `detrendr` 0.4.0

## NEW FEATURES
* Add the option to detrend for the purpose of FCS or FFS.


# `detrendr` 0.3.0

## NEW FEATURES
* Batch processing: detrend an entire folder with the likes of `dir_detrend_exp()`.

## BUG FIXES
* Asymmetric images caused R to crash.
* `NA`s in simulated brightnesses were needlessly causing the automatic parameter-finding routines to fail.


# `detrendr` 0.2.0

## NEW FEATURES
* The process of extending time series prior to smoothing is not done any more. This was introducing errors for images with low counts. Smoothing works fine without it. Dropping this extension gives a massive improvement in detrending speed :-)
* TIFF I/O and image display are now taken care of by the `ijtiff` package.
* Images are now represented in the style of an `ijtiff::ijtiff_img`.

## DEFUNCT
* This package no longer exports functions for TIFF I/O nor image display.


# `detrendr` 0.1.0
* The first CRAN-worthy version.
