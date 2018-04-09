#include <Rcpp.h>
#include <vector>

#include "summary_stats.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector mean_frames(NumericVector arr3d) {
  Dimension d = arr3d.attr("dim");
  std::size_t frame_length = d[0] * d[1];
  NumericVector frame_means(d[2]);
  std::vector<double> fm(d[2]);
  for (std::size_t i = 0; i != d[2]; ++i) {
    frame_means[i] = mymean(arr3d.begin() + i * frame_length,
                            arr3d.begin() + (i + 1) * frame_length);
  }
  return frame_means;
}

// [[Rcpp::export]]
NumericMatrix mat_add_1s(NumericMatrix mat,
                         NumericVector row_adds, NumericVector col_add) {
  // beware that this function may modify mat
  std::size_t nr = mat.nrow();
  std::size_t col_add_pos = 0;
  Rcout << col_add << std::endl;
  Rcout << col_add[col_add_pos] << std::endl;
  for (std::size_t i = 0; i!= nr; ++i) {
    for (std::size_t j = 0; j!= row_adds[i]; ++j)
      Rcout << i << " " << col_add_pos << " " << col_add[col_add_pos] << std::endl;
    ++mat(i, col_add[col_add_pos++]);
  }
  return mat;
}
