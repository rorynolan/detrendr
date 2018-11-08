## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = "#>", 
                      fig.width = 7, fig.height = 6)

## ----load----------------------------------------------------------------
library(detrendr)

## ----visualize bleaching, fig.height=2.6, fig.width=7--------------------
library(magrittr)
path <- system.file("extdata", "bleached.tif", package = "detrendr")
img <- ijtiff::read_tif(path, msg = FALSE)
every100th <- purrr::map(seq(1, dim(img)[4], by = 99), ~ img[, , 1, .]) %>% 
  purrr::reduce(~ cbind(.x, max(img), .y))
ijtiff::display(every100th)

## ----detrend, fig.height=2.6, fig.width=7--------------------------------
system.time(corrected_exp <- img_detrend_exp(img, "auto", purpose = "FFS",
                                             parallel = 2))["elapsed"]
every100th <- purrr::map(seq(1, dim(img)[4], by = 99), 
                         ~ corrected_exp[, , 1, .]) %>% 
  purrr::reduce(~ cbind(.x, max(img), .y))
ijtiff::display(every100th)

## ----boxcar and polynom--------------------------------------------------
system.time(corrected_boxcar <- img_detrend_boxcar(img, "auto", purpose = "FFS",
                                  parallel = 2))["elapsed"]
system.time(corrected_polynom <- img_detrend_polynom(img, "auto", 
                                   purpose = "FFS",
                                   parallel = 2))["elapsed"]

## ----compare brightnesses------------------------------------------------
mean(brightness_pillars(corrected_exp))
mean(brightness_pillars(corrected_boxcar))
mean(brightness_pillars(corrected_polynom))

