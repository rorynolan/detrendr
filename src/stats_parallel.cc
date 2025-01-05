// [[Rcpp::depends(RcppThread)]]

#include <algorithm>
#include <cmath>

#include <Rcpp.h>
#include <RcppThread.h>

using namespace Rcpp;

double square_root_double(double x) {
  return std::sqrt(x);
}

// [[Rcpp::export]]
NumericVector square_root_(NumericVector x) {
  NumericVector output(x.size());
  RcppThread::parallelFor(0, x.size(), [&](std::size_t begin) {
    output[begin] = square_root_double(x[begin]);
  });
  return output;
}
