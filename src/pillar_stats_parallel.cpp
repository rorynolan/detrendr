// [[Rcpp::depends(RcppThread)]]

#include <Rcpp.h>
#include <RcppThread.h>

#include <numeric>

#include "pillar_utils.h"
#include "summary_stats.h"

using namespace Rcpp;

// Helper function to validate array dimensions
void validate_array_3dim(const IntegerVector& dims) {
  if (dims.length() != 3) {
    stop("Input array must be 3-dimensional");
  }
  if (dims[0] == 0 || dims[1] == 0 || dims[2] == 0) {
    stop("Input array dimensions must be non-zero");
  }
}

// [[Rcpp::export]]
NumericMatrix sum_pillars_(NumericVector arr3d) {
  IntegerVector arr3d_dim = arr3d.attr("dim");
  validate_array_3dim(arr3d_dim);

  NumericMatrix output(arr3d_dim[0], arr3d_dim[1]);
  std::size_t nrow = arr3d_dim[0];

  RcppThread::parallelFor(0, arr3d_dim[0] * arr3d_dim[1], [&](std::size_t p) {
    size_t row = p % nrow;
    size_t col = p / nrow;
    std::vector<double> pillar_p = extract_pillar<double>(arr3d, arr3d_dim, p);
    output(row, col) = std::accumulate(pillar_p.begin(), pillar_p.end(), 0.0);
  });

  return output;
}

// [[Rcpp::export]]
NumericMatrix mean_pillars_(NumericVector arr3d) {
  IntegerVector arr3d_dim = arr3d.attr("dim");
  validate_array_3dim(arr3d_dim);

  NumericMatrix output(arr3d_dim[0], arr3d_dim[1]);
  std::size_t nrow = arr3d_dim[0];

  RcppThread::parallelFor(0, arr3d_dim[0] * arr3d_dim[1], [&](std::size_t p) {
    size_t row = p % nrow;
    size_t col = p / nrow;
    std::vector<double> pillar_p = extract_pillar<double>(arr3d, arr3d_dim, p);
    output(row, col) = mymean(pillar_p);
  });

  return output;
}

// [[Rcpp::export]]
NumericMatrix var_pillars_(NumericVector arr3d) {
  IntegerVector arr3d_dim = arr3d.attr("dim");
  validate_array_3dim(arr3d_dim);

  NumericMatrix output(arr3d_dim[0], arr3d_dim[1]);
  std::size_t nrow = arr3d_dim[0];

  RcppThread::parallelFor(0, arr3d_dim[0] * arr3d_dim[1], [&](std::size_t p) {
    size_t row = p % nrow;
    size_t col = p / nrow;
    std::vector<double> pillar_p = extract_pillar<double>(arr3d, arr3d_dim, p);
    output(row, col) = myvar(pillar_p);
  });

  return output;
}

// [[Rcpp::export]]
NumericMatrix median_pillars_(NumericVector arr3d) {
  IntegerVector arr3d_dim = arr3d.attr("dim");
  validate_array_3dim(arr3d_dim);

  NumericMatrix output(arr3d_dim[0], arr3d_dim[1]);
  std::size_t nrow = arr3d_dim[0];

  RcppThread::parallelFor(0, arr3d_dim[0] * arr3d_dim[1], [&](std::size_t p) {
    size_t row = p % nrow;
    size_t col = p / nrow;
    std::vector<double> pillar_p = extract_pillar<double>(arr3d, arr3d_dim, p);
    output(row, col) = mymedian(pillar_p);
  });

  return output;
}

// [[Rcpp::export]]
NumericMatrix brightness_pillars_(NumericVector arr3d) {
  IntegerVector arr3d_dim = arr3d.attr("dim");
  validate_array_3dim(arr3d_dim);

  NumericMatrix output(arr3d_dim[0], arr3d_dim[1]);
  std::size_t nrow = arr3d_dim[0];

  RcppThread::parallelFor(0, arr3d_dim[0] * arr3d_dim[1], [&](std::size_t p) {
    size_t row = p % nrow;
    size_t col = p / nrow;
    std::vector<double> pillar_p = extract_pillar<double>(arr3d, arr3d_dim, p);
    output(row, col) = brightness(pillar_p);
  });

  return output;
}
