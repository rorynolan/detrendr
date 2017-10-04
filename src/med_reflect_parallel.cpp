// [[Rcpp::depends(RcppParallel)]]

#include <vector>

#include <RcppParallel.h>
#include <Rcpp.h>

#include "med_reflect.h"
#include "pillar_utils.h"
#include "summary_stats.h"

using namespace RcppParallel;
using namespace Rcpp;

using std::vector;


struct MedReflectExtendRows : public Worker {
  // source matrix
  RMatrix<double> current;
  RMatrix<double> original;

  std::size_t extend_both_sides_by;

  bool preserve_mean, smooth;

  // destination matrix
  RMatrix<double> output;

  // initialize with source and destination
  MedReflectExtendRows(NumericMatrix current,
                       NumericMatrix original,
                       std::size_t extend_both_sides_by,
                       bool preserve_mean, bool smooth,
                       NumericMatrix output) :
    current(current), original(original),
    extend_both_sides_by(extend_both_sides_by),
    preserve_mean(preserve_mean), smooth(smooth),
    output(output) {}

  // extend the rows
  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i != end; ++i) {
      vector<double> current_row_i(current.row(i).begin(),
                                   current.row(i).end());
      vector<double> original_row_i(original.row(i).begin(),
                                    original.row(i).end());
      vector<double> extended = med_reflect_extend_(current_row_i,
                                                    original_row_i,
                                                    extend_both_sides_by,
                                                    preserve_mean, smooth);
      std::copy(extended.begin(), extended.end(), output.row(i).begin());
    }
  }
};


// [[Rcpp::export]]
NumericMatrix med_reflect_extend_rows_(NumericMatrix current,
                                       NumericMatrix original,
                                       std::size_t extend_both_sides_by,
                                       bool preserve_mean, bool smooth) {

   // allocate the matrix we will return
   NumericMatrix output(current.nrow(),
                        current.ncol() + 2 * extend_both_sides_by);

   // create the worker
   MedReflectExtendRows medReflectExtendRows(current, original,
                                             extend_both_sides_by,
                                             preserve_mean, smooth,
                                             output);

   // call it with parallelFor
   parallelFor(0, current.nrow(), medReflectExtendRows);

   return output;
}


struct MedReflectExtendPillars : public Worker {

  const RVector<double> current;
  const RVector<int> current_dim;

  const RVector<double> original;
  const RVector<int> original_dim;

  const std::size_t extend_both_sides_by;

  const bool preserve_mean, smooth;

  // destination matrix
  RVector<double> output;

  // initialize with source and destination
  MedReflectExtendPillars(const NumericVector current,
                          const IntegerVector current_dim,
                          const NumericVector original,
                          const IntegerVector original_dim,
                          const std::size_t extend_both_sides_by,
                          const bool preserve_mean, const bool smooth,
                          NumericVector output) :
    current(current), current_dim(current_dim),
    original(original), original_dim(original_dim),
    extend_both_sides_by(extend_both_sides_by),
    preserve_mean(preserve_mean), smooth(smooth),
    output(output) {}

  // extend the rows
  void operator()(std::size_t begin, std::size_t end) {
    vector<double> current_pillar(current_dim[2]);
    vector<double> original_pillar(original_dim[2]);
    std::vector<int> output_dim = {current_dim[0], current_dim[1],
                                   current_dim[2] +
                                     2 * (int) extend_both_sides_by};
    for (std::size_t p = begin; p != end; ++p) {
      current_pillar = extract_pillar<double>(current, current_dim, p);
      original_pillar = extract_pillar<double>(original, original_dim, p);
      vector<double> extended = med_reflect_extend_(current_pillar,
                                                    original_pillar,
                                                    extend_both_sides_by,
                                                    preserve_mean, smooth);
      assign_pillar(output, output_dim, extended, p);
    }
  }
};

// [[Rcpp::export]]
NumericVector med_reflect_extend_pillars_(NumericVector current,
                                          NumericVector original,
                                          std::size_t extend_both_sides_by,
                                          bool preserve_mean = false,
                                          bool smooth = false) {
  IntegerVector current_dim = current.attr("dim");
  IntegerVector original_dim = original.attr("dim");

  IntegerVector output_dim = IntegerVector::create(
                               current_dim[0], current_dim[1],
                               current_dim[2] + 2 * extend_both_sides_by);

  // allocate the matrix we will return
  NumericVector output(myprod(output_dim));

  // create the worker
  MedReflectExtendPillars medReflectExtendPillars(current, current_dim,
                                                  original, original_dim,
                                                  extend_both_sides_by,
                                                  preserve_mean, smooth,
                                                  output);

  // call it with parallelFor
  parallelFor(0, current_dim[0] * current_dim[1], medReflectExtendPillars);

  output.attr("dim") = output_dim;

  return output;
}

