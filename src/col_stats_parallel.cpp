// [[Rcpp::depends(RcppParallel)]]

#include <vector>

#include <RcppParallel.h>
#include <Rcpp.h>

#include "summary_stats.h"

using namespace RcppParallel;
using namespace Rcpp;

using std::vector;


struct BrightnessCols : public Worker {

  RMatrix<int> cols;

  // destination
  RVector<double> output;

  // initialize with source and destination
  BrightnessCols(IntegerMatrix cols, NumericVector output) :
    cols(cols), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i != end; ++i) {
      const vector<int> col_i(cols.column(i).begin(), cols.column(i).end());
        output[i] = brightness(col_i);
    }
  }
};

// [[Rcpp::export]]
NumericVector brightness_cols_(IntegerMatrix cols) {

  std::size_t ncol = cols.ncol();

  NumericVector output(ncol);

  // create the worker
  BrightnessCols brightnessCols(cols, output);

  // call it with parallelFor
  parallelFor(0, ncol, brightnessCols);

  return output;
}

struct MeanCols : public Worker {

  RMatrix<int> cols;

  // destination
  RVector<double> output;

  // initialize with source and destination
  MeanCols(IntegerMatrix cols, NumericVector output) :
    cols(cols), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i != end; ++i) {
      const vector<int> col_i(cols.column(i).begin(), cols.column(i).end());
      output[i] = mymean(col_i);
    }
  }
};

// [[Rcpp::export]]
NumericVector mean_cols_(IntegerMatrix cols) {

  std::size_t ncol = cols.ncol();

  NumericVector output(ncol);

  // create the worker
  MeanCols meanCols(cols, output);

  // call it with parallelFor
  parallelFor(0, ncol, meanCols);

  return output;
}
