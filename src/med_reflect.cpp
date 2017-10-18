#include <vector>
#include <algorithm>

#include <Rcpp.h>

#include "med_reflect.h"

using namespace Rcpp;

using std::vector;

// [[Rcpp::export]]
NumericVector med_reflect_extend_(NumericVector vec, NumericVector orig,
                                  std::size_t extend_both_sides_by,
                                  bool preserve_mean = false,
                                  bool smooth = false) {
  vector<double> vec_stdv = as<vector<double> >(vec);
  vector<double> orig_stdv = as<vector<double> >(orig);
  vector<double> out = med_reflect_extend_(vec_stdv, orig_stdv,
                                           extend_both_sides_by,
                                           preserve_mean, smooth);
  return wrap(out);
}

