// [[Rcpp::depends(RcppThread)]]

#include <vector>
#include <algorithm>
#include <cmath>
#include <numeric>

#include <RcppThread.h>
#include <Rcpp.h>

#include "pillar_utils.h"
#include "smooth.h"
#include "summary_stats.h"

using namespace Rcpp;
using std::vector;

// [[Rcpp::export]]
NumericMatrix boxcar_smooth_rows(NumericMatrix mat, std::size_t l) {
  if (mat.nrow() == 0 || mat.ncol() == 0) {
    return NumericMatrix(mat.nrow(), mat.ncol());
  }

  NumericMatrix output(mat.nrow(), mat.ncol());

  auto worker = [&](std::size_t i) {
    vector<double> row_i(mat.row(i).begin(), mat.row(i).end());
    vector<double> smoothed = boxcar_smooth<vector<double>>(row_i, l);
    std::copy(smoothed.begin(), smoothed.end(), output.row(i).begin());
  };

  RcppThread::parallelFor(0, mat.nrow(), worker);
  return output;
}

// [[Rcpp::export]]
NumericVector boxcar_smooth_pillars(NumericVector arr, std::size_t l) {
  IntegerVector arr_dim = arr.attr("dim");
  if (arr_dim.length() < 3) {
    stop("Input array must have 3 dimensions");
  }

  NumericVector output(myprod(arr_dim));
  output.attr("dim") = arr_dim;

  auto worker = [&](std::size_t p) {
    vector<double> arr_pillar = extract_pillar<double>(arr, arr_dim, p);
    vector<double> smoothed_pillar = boxcar_smooth<vector<double>>(arr_pillar, l);
    assign_pillar(output, arr_dim, smoothed_pillar, p);
  };

  RcppThread::parallelFor(0, arr_dim[0] * arr_dim[1], worker);
  return output;
}

// [[Rcpp::export]]
NumericMatrix exp_smooth_rows(NumericMatrix mat, double tau, std::size_t l) {
  if (mat.nrow() == 0 || mat.ncol() == 0) {
    return NumericMatrix(mat.nrow(), mat.ncol());
  }

  NumericMatrix output(mat.nrow(), mat.ncol());

  // Pre-calculate weights
  vector<double> weights(2 * l + 1);
  weights[l] = 1;
  for (std::size_t i = 1; i != l + 1; ++i) {
    weights[l - i] = weights[l + i] = std::exp(- (double) i / tau);
  }

  auto worker = [&](std::size_t i) {
    vector<double> row_i(mat.row(i).begin(), mat.row(i).end());
    vector<double> smoothed = weighted_smooth<vector<double>>(row_i, weights);
    std::copy(smoothed.begin(), smoothed.end(), output.row(i).begin());
  };

  RcppThread::parallelFor(0, mat.nrow(), worker);
  return output;
}

// [[Rcpp::export]]
NumericVector exp_smooth_pillars(NumericVector arr, double tau, int l) {
  IntegerVector arr_dim = arr.attr("dim");
  if (arr_dim.length() < 3) {
    stop("Input array must have 3 dimensions");
  }

  NumericVector output(myprod(arr_dim));
  output.attr("dim") = arr_dim;

  // Pre-calculate weights
  vector<double> weights(2 * l + 1);
  weights[l] = 1;
  for (int i = 1; i != l + 1; ++i) {
    weights[l - i] = weights[l + i] = std::exp(- i / tau);
  }

  auto worker = [&](std::size_t p) {
    vector<double> arr_pillar = extract_pillar<double>(arr, arr_dim, p);
    vector<double> smoothed_pillar = weighted_smooth<vector<double>>(arr_pillar, weights);
    assign_pillar(output, arr_dim, smoothed_pillar, p);
  };

  RcppThread::parallelFor(0, arr_dim[0] * arr_dim[1], worker);
  return output;
}
