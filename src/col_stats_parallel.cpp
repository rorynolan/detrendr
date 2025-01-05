// [[Rcpp::depends(RcppThread)]]

#include <vector>
#include <numeric>
#include <Rcpp.h>
#include <RcppThread.h>

#include "summary_stats.h"

using namespace Rcpp;
using std::vector;

// [[Rcpp::export]]
NumericVector brightness_cols(IntegerMatrix cols) {
  std::size_t ncol = cols.ncol();
  std::size_t nrow = cols.nrow();
  if (ncol == 0) {
    return NumericVector(0);
  }

  // Convert matrix to vector - R matrices are column-major
  vector<int> data(cols.begin(), cols.end());
  vector<double> temp_output(ncol);

  auto worker = [&data, &temp_output, nrow](std::size_t i) {
    temp_output[i] = brightness(
      data.begin() + i * nrow,
      data.begin() + (i + 1) * nrow
    );
  };

  RcppThread::parallelFor(0, ncol, worker);
  return NumericVector(temp_output.begin(), temp_output.end());
}

// [[Rcpp::export]]
NumericVector brightness_cols_given_mean(IntegerMatrix cols, NumericVector means) {
  std::size_t ncol = cols.ncol();
  std::size_t nrow = cols.nrow();
  if (ncol == 0) {
    return NumericVector(0);
  }

  if (means.length() != ncol) {
    stop("Length of means must match number of columns");
  }

  // Convert matrix to vector - R matrices are column-major
  vector<int> data(cols.begin(), cols.end());
  vector<double> temp_output(ncol);
  vector<double> means_vec(means.begin(), means.end());

  auto worker = [&data, &temp_output, &means_vec, nrow](std::size_t i) {
    temp_output[i] = brightness(
      data.begin() + i * nrow,
      data.begin() + (i + 1) * nrow,
      means_vec[i]
    );
  };

  RcppThread::parallelFor(0, ncol, worker);
  return NumericVector(temp_output.begin(), temp_output.end());
}

// [[Rcpp::export]]
NumericVector var_cols_given_mean(IntegerMatrix cols, NumericVector means) {
  std::size_t ncol = cols.ncol();
  std::size_t nrow = cols.nrow();
  if (ncol == 0) {
    return NumericVector(0);
  }

  if (means.length() != ncol) {
    stop("Length of means must match number of columns");
  }

  // Convert matrix to vector - R matrices are column-major
  vector<int> data(cols.begin(), cols.end());
  vector<double> temp_output(ncol);
  vector<double> means_vec(means.begin(), means.end());

  auto worker = [&data, &temp_output, &means_vec, nrow](std::size_t i) {
    temp_output[i] = myvar(
      data.begin() + i * nrow,
      data.begin() + (i + 1) * nrow,
      means_vec[i]
    );
  };

  RcppThread::parallelFor(0, ncol, worker);
  return NumericVector(temp_output.begin(), temp_output.end());
}
