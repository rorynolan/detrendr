#include "summary_stats.h"

bool int_anyNA(const std::vector<int>& x) {
  return std::any_of(x.begin(), x.end(), [](int val) { return Rcpp::IntegerVector::is_na(val); });
}

bool dbl_anyNA(const std::vector<double>& x) {
  return std::any_of(x.begin(), x.end(), [](double val) { return Rcpp::NumericVector::is_na(val); });
}
