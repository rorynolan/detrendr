// [[Rcpp::depends(RcppParallel)]]

#include <vector>

#include <RcppParallel.h>
#include <Rcpp.h>

#include "summary_stats.h"

using namespace RcppParallel;
using namespace Rcpp;

using std::vector;


struct BrightnessRows : public Worker {

  RMatrix<int> rows;

  // destination
  RVector<double> output;

  // initialize with source and destination
  BrightnessRows(IntegerMatrix rows, NumericVector output) :
    rows(rows), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i != end; ++i) {
      const vector<int> row_i(rows.row(i).begin(), rows.row(i).end());
        output[i] = brightness(row_i);
    }
  }
};

// [[Rcpp::export]]
NumericVector brightness_rows_(IntegerMatrix rows) {

  std::size_t nrow = rows.nrow();

  NumericVector output(nrow);

  // create the worker
  BrightnessRows brightnessRows(rows, output);

  // call it with parallelFor
  parallelFor(0, nrow, brightnessRows);

  return output;
}

struct MeanRows : public Worker {

  RMatrix<int> rows;

  // destination
  RVector<double> output;

  // initialize with source and destination
  MeanRows(IntegerMatrix rows, NumericVector output) :
    rows(rows), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i != end; ++i) {
      const vector<int> row_i(rows.row(i).begin(), rows.row(i).end());
      output[i] = mymean(row_i);
    }
  }
};

// [[Rcpp::export]]
NumericVector mean_rows_(IntegerMatrix rows) {

  std::size_t nrow = rows.nrow();

  NumericVector output(nrow);

  // create the worker
  MeanRows meanRows(rows, output);

  // call it with parallelFor
  parallelFor(0, nrow, meanRows);

  return output;
}
