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
