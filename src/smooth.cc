#include <Rcpp.h>

#include "smooth.h"

using namespace Rcpp;


// [[Rcpp::export]]
NumericVector boxcar_smooth(NumericVector extended, std::size_t l) {
  return boxcar_smooth<NumericVector>(extended, l);
}

// [[Rcpp::export]]
NumericVector weighted_smooth(NumericVector extended, NumericVector weights) {
  return weighted_smooth<NumericVector>(extended, weights);
}

// [[Rcpp::export]]
NumericVector exp_smooth(NumericVector extended, double tau, std::size_t l) {
  return exp_smooth<NumericVector>(extended, tau, l);
}
