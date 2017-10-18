// [[Rcpp::depends(RcppParallel)]]

#include <vector>

#include "polynom.h"

#include <Rcpp.h>
#include <RcppParallel.h>

using namespace Rcpp;
using namespace RcppParallel;

using std::vector;


struct PolyCalcCoefCols : public Worker {

  RVector<double> x;
  RMatrix<double> coefs;

  // destination matrix
  RMatrix<double> output;

  // initialize with source and destination
  PolyCalcCoefCols(NumericVector x, NumericMatrix coefs,
                   NumericMatrix output) :
    x(x), coefs(coefs), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    vector<double> x_std(x.begin(), x.end());
    for (std::size_t i = begin; i != end; ++i) {
      vector<double> coefs_i(coefs.column(i).begin(), coefs.column(i).end());
      vector<double> poly_y = poly_calc(x_std, coefs_i);
      std::copy(poly_y.begin(), poly_y.end(), output.column(i).begin());
    }
  }
};


// [[Rcpp::export]]
NumericMatrix poly_calc_coef_cols_(NumericVector x, NumericMatrix coefs) {

  // allocate the matrix we will return
  NumericMatrix output(x.size(), coefs.ncol());

  // create the worker
  PolyCalcCoefCols polyCalcCoefCols(x, coefs, output);

  // call it with parallelFor
  parallelFor(0, coefs.ncol(), polyCalcCoefCols);

  return output;
}
