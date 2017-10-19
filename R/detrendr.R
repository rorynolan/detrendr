#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom Rcpp sourceCpp
#' @importFrom magrittr '%>%' '%T>%' '%<>%'
#' @importFrom foreach '%dopar%'
#' @importFrom grDevices adjustcolor
#' @useDynLib detrendr, .registration = TRUE
NULL

## quiet concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "var", "maxl", "l", "seed"))
}
