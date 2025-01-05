// [[Rcpp::depends(RcppParallel)]]

#include <vector>
#include <algorithm>

#include <Rcpp.h>

#include "pillar_utils.h"
#include "summary_stats.h"

using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix pillars_to_cols(NumericVector arr3d) {
  IntegerVector arr3d_dim = arr3d.attr("dim");
  std::size_t n_pillars = arr3d_dim[0] * arr3d_dim[1];
  NumericMatrix output(arr3d_dim[2], n_pillars);
  for (std::size_t p = 0; p != n_pillars; ++p) {
    for (std::size_t slice = 0; slice != arr3d_dim[2]; ++slice) {
      output(slice, p) = arr3d[slice * n_pillars + p];
    }
  }
  return output;
}

// [[Rcpp::export]]
NumericVector cols_to_pillars(NumericMatrix mat,
                              IntegerVector output_dim) {
  NumericVector output(myprod(output_dim));
  for (std::size_t p = 0; p != output_dim[0] * output_dim[1]; ++p) {
    for (std::size_t slice = 0; slice != output_dim[2]; ++slice) {
      output[slice * output_dim[0] * output_dim[1] + p] = mat(slice, p);
    }
  }
  output.attr("dim") = output_dim;
  return output;
}
