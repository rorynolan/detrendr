// [[Rcpp::depends(RcppThread)]]

#include <vector>
#include <numeric>
#include <Rcpp.h>
#include <RcppThread.h>

#include "summary_stats.h"

using namespace Rcpp;
using std::vector;

// Helper function to extract a row from column-major matrix
template <typename T>
vector<T> extract_row(const vector<T>& data, std::size_t row, std::size_t nrow, std::size_t ncol) {
  vector<T> result(ncol);
  for (std::size_t j = 0; j < ncol; ++j) {
    result[j] = data[j * nrow + row];
  }
  return result;
}

// [[Rcpp::export]]
NumericVector brightness_rows(IntegerMatrix rows) {
  std::size_t ncol = rows.ncol();
  std::size_t nrow = rows.nrow();
  if (nrow == 0) {
    return NumericVector(0);
  }

  // Convert matrix to vector - R matrices are column-major
  vector<int> data(rows.begin(), rows.end());
  vector<double> temp_output(nrow);

  auto worker = [&data, &temp_output, nrow, ncol](std::size_t i) {
    vector<int> row = extract_row(data, i, nrow, ncol);
    temp_output[i] = brightness(row.begin(), row.end());
  };

  RcppThread::parallelFor(0, nrow, worker);
  return NumericVector(temp_output.begin(), temp_output.end());
}

// [[Rcpp::export]]
NumericVector brightness_rows_given_mean(IntegerMatrix rows, NumericVector means) {
  std::size_t ncol = rows.ncol();
  std::size_t nrow = rows.nrow();
  if (nrow == 0) {
    return NumericVector(0);
  }

  if (means.length() != nrow) {
    stop("Length of means must match number of rows");
  }

  // Convert matrix to vector - R matrices are column-major
  vector<int> data(rows.begin(), rows.end());
  vector<double> temp_output(nrow);
  vector<double> means_vec(means.begin(), means.end());

  auto worker = [&data, &temp_output, &means_vec, nrow, ncol](std::size_t i) {
    vector<int> row = extract_row(data, i, nrow, ncol);
    temp_output[i] = brightness(row.begin(), row.end(), means_vec[i]);
  };

  RcppThread::parallelFor(0, nrow, worker);
  return NumericVector(temp_output.begin(), temp_output.end());
}

// [[Rcpp::export]]
NumericVector var_rows_given_mean(IntegerMatrix rows, NumericVector means) {
  std::size_t ncol = rows.ncol();
  std::size_t nrow = rows.nrow();
  if (nrow == 0) {
    return NumericVector(0);
  }

  if (means.length() != nrow) {
    stop("Length of means must match number of rows");
  }

  // Convert matrix to vector - R matrices are column-major
  vector<int> data(rows.begin(), rows.end());
  vector<double> temp_output(nrow);
  vector<double> means_vec(means.begin(), means.end());

  auto worker = [&data, &temp_output, &means_vec, nrow, ncol](std::size_t i) {
    vector<int> row = extract_row(data, i, nrow, ncol);
    temp_output[i] = myvar(row.begin(), row.end(), means_vec[i]);
  };

  RcppThread::parallelFor(0, nrow, worker);
  return NumericVector(temp_output.begin(), temp_output.end());
}

// [[Rcpp::export]]
NumericVector sum_rows(IntegerMatrix rows) {
  std::size_t ncol = rows.ncol();
  std::size_t nrow = rows.nrow();
  if (nrow == 0) {
    return NumericVector(0);
  }

  // Convert matrix to vector - R matrices are column-major
  vector<int> data(rows.begin(), rows.end());
  vector<double> temp_output(nrow);

  auto worker = [&data, &temp_output, nrow, ncol](std::size_t i) {
    vector<int> row = extract_row(data, i, nrow, ncol);
    temp_output[i] = std::accumulate(row.begin(), row.end(), 0.0);
  };

  RcppThread::parallelFor(0, nrow, worker);
  return NumericVector(temp_output.begin(), temp_output.end());
}
