#include <Rcpp.h>

#include "smooth.h"

using namespace Rcpp;


// [[Rcpp::export]]
NumericVector boxcar_smooth(NumericVector extended,
                            std::size_t extended_both_sides_by,
                            std::size_t l) {
  return boxcar_smooth<NumericVector>(extended, extended_both_sides_by, l);
}

// [[Rcpp::export]]
NumericVector weighted_smooth(NumericVector extended,
                              std::size_t extended_both_sides_by,
                              NumericVector weights, double weight_sum) {
  return weighted_smooth<NumericVector>(extended,
                                        extended_both_sides_by,
                                        weights, weight_sum);
}

// [[Rcpp::export]]
NumericVector exp_smooth(NumericVector extended,
                         std::size_t extended_both_sides_by,
                         double tau, std::size_t l) {
  return exp_smooth<NumericVector>(extended, extended_both_sides_by, tau, l);
}
