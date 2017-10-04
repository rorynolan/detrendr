// [[Rcpp::depends(RcppParallel)]]

#include <vector>
#include <algorithm>
#include <cmath>
#include <numeric>

#include <RcppParallel.h>
#include <Rcpp.h>

#include "smooth.h"
#include "pillar_utils.h"
#include "summary_stats.h"

using namespace RcppParallel;
using namespace Rcpp;

using std::vector;


struct BoxcarSmoothRows : public Worker {
  // source matrix
  RMatrix<double> extended;

  const std::size_t extended_both_sides_by, l;

  // destination matrix
  RMatrix<double> output;

  // initialize with source and destination
  BoxcarSmoothRows(NumericMatrix extended,
                   std::size_t extended_both_sides_by, std::size_t l,
                   NumericMatrix output) :
    extended(extended), extended_both_sides_by(extended_both_sides_by), l(l),
    output(output) {}

  // extend the rows
  void operator()(std::size_t begin, std::size_t end) {
    vector<double> smoothed(extended.ncol() - 2 * extended_both_sides_by);
    for (std::size_t i = begin; i != end; ++i) {
      vector<double> row_i(extended.row(i).begin(), extended.row(i).end());
      smoothed = boxcar_smooth<vector<double> >(row_i,
                                                extended_both_sides_by, l);
      std::copy(smoothed.begin(), smoothed.end(), output.row(i).begin());
    }
  }
};


// [[Rcpp::export]]
NumericMatrix boxcar_smooth_rows_(NumericMatrix extended,
                                  std::size_t extended_both_sides_by,
                                  std::size_t l) {

  // allocate the matrix we will return
  NumericMatrix output(extended.nrow(),
                       extended.ncol() - 2 * extended_both_sides_by);

  // create the worker
  BoxcarSmoothRows boxcarSmoothRows(extended, extended_both_sides_by, l,
                                    output);

  // call it with parallelFor
  parallelFor(0, extended.nrow(), boxcarSmoothRows);

  return output;
}


struct BoxcarSmoothPillars : public Worker {

  const RVector<double> extended;
  const RVector<int> extended_dim;

  const std::size_t extended_both_sides_by, l;

  // destination matrix
  RVector<double> output;

  // initialize with source and destination
  BoxcarSmoothPillars(NumericVector extended, IntegerVector extended_dim,
                      std::size_t extended_both_sides_by, std::size_t l,
                      NumericVector output) :
    extended(extended), extended_dim(extended_dim),
    extended_both_sides_by(extended_both_sides_by), l(l),
    output(output) {}

  // extend the rows
  void operator()(std::size_t begin, std::size_t end) {
    vector<double> extended_pillar(extended_dim[2]);
    std::vector<int> output_dim = {extended_dim[0], extended_dim[1],
                                   extended_dim[2] -
                                     2 * (int) extended_both_sides_by};
    vector<double> smoothed_pillar(output_dim[2]);
    for (std::size_t p = begin; p != end; ++p) {
      extended_pillar = extract_pillar<double>(extended, extended_dim, p);
      smoothed_pillar = boxcar_smooth<vector<double> >(extended_pillar,
                                                       extended_both_sides_by,
                                                       l);
      assign_pillar(output, output_dim, smoothed_pillar, p);
    }
  }
};

// [[Rcpp::export]]
NumericVector boxcar_smooth_pillars_(NumericVector extended,
                                     std::size_t extended_both_sides_by,
                                     std::size_t l) {
  IntegerVector extended_dim = extended.attr("dim");

  IntegerVector output_dim = IntegerVector::create(
    extended_dim[0], extended_dim[1],
    extended_dim[2] - 2 * extended_both_sides_by
  );

  // allocate the matrix we will return
  NumericVector output(myprod(output_dim));

  // create the worker
  BoxcarSmoothPillars boxcarSmoothPillars(extended, extended_dim,
                                          extended_both_sides_by, l,
                                          output);

  // call it with parallelFor
  parallelFor(0, extended_dim[0] * extended_dim[1], boxcarSmoothPillars);

  output.attr("dim") = output_dim;

  return output;
}



struct ExpSmoothRows : public Worker {
  // source matrix
  RMatrix<double> extended;

  const std::size_t extended_both_sides_by;

  const double tau;

  const int l;

  // destination matrix
  RMatrix<double> output;

  // initialize with source and destination
  ExpSmoothRows(NumericMatrix extended,
                std::size_t extended_both_sides_by, double tau,
                int l, NumericMatrix output) :
    extended(extended), extended_both_sides_by(extended_both_sides_by),
    tau(tau), l(l), output(output) {}

  // extend the rows
  void operator()(std::size_t begin, std::size_t end) {
    vector<double> smoothed(extended.ncol() - 2 * extended_both_sides_by);
    vector<double> weights(2 * l + 1);
    weights[l] = 1;
    for (std::size_t i = 1; i != l + 1; ++i) {
      weights[l - i] = weights[l + i] = std::exp(- (double) i / tau);
    }
    double weight_sum = std::accumulate(weights.begin(), weights.end(), 0.0);
    for (std::size_t i = begin; i != end; ++i) {
      vector<double> row_i(extended.row(i).begin(), extended.row(i).end());
      smoothed = weighted_smooth<vector<double> >(row_i,
                                                  extended_both_sides_by,
                                                  weights, weight_sum);
      std::copy(smoothed.begin(), smoothed.end(), output.row(i).begin());
    }
  }
};


// [[Rcpp::export]]
NumericMatrix exp_smooth_rows_(NumericMatrix extended,
                               std::size_t extended_both_sides_by,
                               double tau, int l) {

  // allocate the matrix we will return
  NumericMatrix output(extended.nrow(),
                       extended.ncol() - 2 * extended_both_sides_by);

  // create the worker
  ExpSmoothRows expSmoothRows(extended, extended_both_sides_by, tau, l,
                              output);

  // call it with parallelFor
  parallelFor(0, extended.nrow(), expSmoothRows);

  return output;
}


struct ExpSmoothPillars : public Worker {

  RVector<double> extended;
  const RVector<int> extended_dim;

  const std::size_t extended_both_sides_by;

  const double tau;

  const int l;

  // destination matrix
  RVector<double> output;

  // initialize with source and destination
  ExpSmoothPillars(NumericVector extended, IntegerVector extended_dim,
                   std::size_t extended_both_sides_by, double tau,
                   int l, NumericVector output) :
    extended(extended), extended_dim(extended_dim),
    extended_both_sides_by(extended_both_sides_by),
    tau(tau), l(l), output(output) {}

  // extend the rows
  void operator()(std::size_t begin, std::size_t end) {
    vector<double> extended_pillar(extended_dim[2]);
    std::vector<int> output_dim = {extended_dim[0], extended_dim[1],
                                   extended_dim[2] -
                                     2 * (int) extended_both_sides_by};
    vector<double> smoothed_pillar(output_dim[2]);
    vector<double> weights(2 * l + 1);
    weights[l] = 1;
    for (int i = 1; i != l + 1; ++i) {
      weights[l - i] = weights[l + i] = std::exp(- i / tau);
    }
    double weight_sum = std::accumulate(weights.begin(), weights.end(), 0.0);
    for (std::size_t p = begin; p != end; ++p) {
      extended_pillar = extract_pillar<double>(extended, extended_dim, p);
      smoothed_pillar = weighted_smooth<vector<double> >(extended_pillar,
                                                         extended_both_sides_by,
                                                         weights, weight_sum);
      assign_pillar(output, output_dim, smoothed_pillar, p);
    }
  }
};

// [[Rcpp::export]]
NumericVector exp_smooth_pillars_(NumericVector extended,
                                  std::size_t extended_both_sides_by,
                                  double tau, int l) {
  IntegerVector extended_dim = extended.attr("dim");

  IntegerVector output_dim = IntegerVector::create(
    extended_dim[0], extended_dim[1],
    extended_dim[2] - 2 * extended_both_sides_by);

  // allocate the matrix we will return
  NumericVector output(myprod(output_dim));

  // create the worker
  ExpSmoothPillars expSmoothPillars(extended, extended_dim,
                                    extended_both_sides_by, tau, l,
                                    output);

  // call it with parallelFor
  parallelFor(0, extended_dim[0] * extended_dim[1], expSmoothPillars);

  output.attr("dim") = output_dim;

  return output;
}
