// [[Rcpp::depends(RcppParallel)]]

#include <Rcpp.h>
#include <RcppParallel.h>

#include "summary_stats.h"

using namespace Rcpp;
using namespace RcppParallel;


struct MeanPillars : public Worker {

  RVector<double> arr3d;
  RVector<int> arr3d_dim;

  RMatrix<double> output;

  // initialize with source and destination
  MeanPillars(NumericVector arr3d, IntegerVector arr3d_dim,
              NumericMatrix output) :
    arr3d(arr3d), arr3d_dim(arr3d_dim), output(output) {}

  // extend the rows
  void operator()(std::size_t begin, std::size_t end) {
    std::size_t nrow = arr3d_dim[0];
    std::size_t ncol = arr3d_dim[1];
    std::size_t nslice = arr3d_dim[2];
    for (std::size_t p = begin; p != end; ++p) {
      size_t row = p % arr3d_dim[1];
      size_t col = p / arr3d_dim[1];
      double mean = 0;
      for (std::size_t slice = 0; slice != nslice; ++slice) {
        mean += arr3d[slice * ncol * nrow + col * nrow + row];
      }
      mean /= nslice;
      output(row, col) = mean;
    }
  }
};

// [[Rcpp::export]]
NumericMatrix mean_pillars_(NumericVector arr3d) {

  IntegerVector arr3d_dim = arr3d.attr("dim");

  NumericMatrix output(arr3d_dim[0], arr3d_dim[1]);

  // create the worker
  MeanPillars meanPillars(arr3d, arr3d_dim, output);

  // call it with parallelFor
  parallelFor(0, arr3d_dim[0] * arr3d_dim[1], meanPillars);

  return output;
}


struct VarPillars : public Worker {

  RVector<double> arr3d;
  RVector<int> arr3d_dim;

  RMatrix<double> output;

  // initialize with source and destination
  VarPillars(NumericVector arr3d, IntegerVector arr3d_dim,
             NumericMatrix output) :
    arr3d(arr3d), arr3d_dim(arr3d_dim), output(output) {}

  // extend the rows
  void operator()(std::size_t begin, std::size_t end) {
    std::size_t nrow = arr3d_dim[0];
    std::size_t ncol = arr3d_dim[1];
    std::size_t nslice = arr3d_dim[2];
    std::vector<double> pillar(nslice);
    for (std::size_t p = begin; p != end; ++p) {
      size_t row = p % arr3d_dim[1];
      size_t col = p / arr3d_dim[1];
      for (std::size_t slice = 0; slice != nslice; ++slice) {
        pillar[slice] = arr3d[slice * ncol * nrow + col * nrow + row];
      }
      output(row, col) = myvar(pillar);
    }
  }
};

// [[Rcpp::export]]
NumericMatrix var_pillars_(NumericVector arr3d) {

  IntegerVector arr3d_dim = arr3d.attr("dim");

  NumericMatrix output(arr3d_dim[0], arr3d_dim[1]);

  // create the worker
  VarPillars varPillars(arr3d, arr3d_dim, output);

  // call it with parallelFor
  parallelFor(0, arr3d_dim[0] * arr3d_dim[1], varPillars);

  return output;
}


struct MedianPillars : public Worker {

  RVector<double> arr3d;
  RVector<int> arr3d_dim;

  RMatrix<double> output;

  // initialize with source and destination
  MedianPillars(NumericVector arr3d, IntegerVector arr3d_dim,
                NumericMatrix output) :
    arr3d(arr3d), arr3d_dim(arr3d_dim), output(output) {}

  // extend the rows
  void operator()(std::size_t begin, std::size_t end) {
    std::size_t nrow = arr3d_dim[0];
    std::size_t ncol = arr3d_dim[1];
    std::size_t nslice = arr3d_dim[2];
    std::vector<double> pillar(nslice);
    for (std::size_t p = begin; p != end; ++p) {
      size_t row = p % arr3d_dim[1];
      size_t col = p / arr3d_dim[1];
      for (std::size_t slice = 0; slice != nslice; ++slice) {
        pillar[slice] = arr3d[slice * ncol * nrow + col * nrow + row];
      }
      output(row, col) = mymedian(pillar);
    }
  }
};

// [[Rcpp::export]]
NumericMatrix median_pillars_(NumericVector arr3d) {

  IntegerVector arr3d_dim = arr3d.attr("dim");

  NumericMatrix output(arr3d_dim[0], arr3d_dim[1]);

  // create the worker
  MedianPillars medianPillars(arr3d, arr3d_dim, output);

  // call it with parallelFor
  parallelFor(0, arr3d_dim[0] * arr3d_dim[1], medianPillars);

  return output;
}


struct BrightnessPillars : public Worker {

  RVector<double> arr3d;
  RVector<int> arr3d_dim;

  RMatrix<double> output;

  // initialize with source and destination
  BrightnessPillars(NumericVector arr3d, IntegerVector arr3d_dim,
                    NumericMatrix output) :
    arr3d(arr3d), arr3d_dim(arr3d_dim), output(output) {}

  // extend the rows
  void operator()(std::size_t begin, std::size_t end) {
    std::size_t nrow = arr3d_dim[0];
    std::size_t ncol = arr3d_dim[1];
    std::size_t nslice = arr3d_dim[2];
    std::vector<double> pillar(nslice);
    for (std::size_t p = begin; p != end; ++p) {
      size_t row = p % arr3d_dim[1];
      size_t col = p / arr3d_dim[1];
      for (std::size_t slice = 0; slice != nslice; ++slice) {
        pillar[slice] = arr3d[slice * ncol * nrow + col * nrow + row];
      }
      output(row, col) = myvar(pillar) / mymean(pillar);
    }
  }
};

// [[Rcpp::export]]
NumericMatrix brightness_pillars_(NumericVector arr3d) {

  IntegerVector arr3d_dim = arr3d.attr("dim");

  NumericMatrix output(arr3d_dim[0], arr3d_dim[1]);

  // create the worker
  BrightnessPillars brightnessPillars(arr3d, arr3d_dim, output);

  // call it with parallelFor
  parallelFor(0, arr3d_dim[0] * arr3d_dim[1], brightnessPillars);

  return output;
}
