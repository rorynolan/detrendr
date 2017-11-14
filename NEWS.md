### 0.2.0

#### NEW FEATURES
* The process of extending time series prior to smoothing is not done any more. This was introducing errors for images with low counts. Smoothing works fine without it. This gives a massive improvement in detrending speed.

#### BUG FIXES
* Fix bug in file names when `write_tif()` writes multi-channel images.

# detrendr 0.1.0
* The first CRAN-worthy version.



