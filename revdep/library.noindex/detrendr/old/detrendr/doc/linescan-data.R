## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----read-linescan-------------------------------------------------------
linescan_img <- ijtiff::read_tif(system.file("extdata", "linescan.tif",
                                             package = "detrendr"))
dim(linescan_img)

## ----convert-from-linescan-----------------------------------------------
converted_img <- ijtiff::linescan_to_stack(linescan_img)
dim(converted_img)

## ----detrend-------------------------------------------------------------
library(detrendr)
detrended_converted_img <- img_detrend_rh(converted_img)
dim(detrended_converted_img)

## ----back-to-linescan----------------------------------------------------
linescan_detrended_img <- ijtiff::stack_to_linescan(detrended_converted_img)
dim(linescan_detrended_img)

