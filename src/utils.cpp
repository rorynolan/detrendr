#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix mat_add1s(IntegerMatrix mat, IntegerMatrix add_pos) {
  // beware that this function modifies its input `mat` in the parent environment
  std::size_t npos = add_pos.nrow();
  for (std::size_t i = 0; i != npos; ++i)
    mat(add_pos(i, 0) - 1, add_pos(i, 1) - 1) += 1;
  return mat;
}

// [[Rcpp::export]]
IntegerVector vec_add1s(IntegerVector vec, IntegerVector add_pos) {
  // beware that this function modifies its input `arr3d` in the parent env
  Dimension d = vec.attr("dim");
  std::size_t npos = add_pos.size();
  for (std::size_t i = 0; i != npos; ++i)
    vec[add_pos[i] - 1] += 1;
  vec.attr("dim") = d;
  return vec;
}

// Convert std::vector<std::vector<int>> to IntegerMatrix with column-major layout
// (each inner vector represents a column)
IntegerMatrix vec_to_matrix_colmajor(const std::vector<std::vector<int>>& vec) {
  if (vec.empty()) {
    return IntegerMatrix(0, 0);
  }
  std::size_t ncol = vec.size();
  std::size_t nrow = vec[0].size();
  IntegerMatrix out(nrow, ncol);
  for (std::size_t i = 0; i < ncol; ++i) {
    for (std::size_t j = 0; j < nrow; ++j) {
      out(j, i) = vec[i][j];
    }
  }
  return out;
}

// Convert std::vector<std::vector<int>> to IntegerMatrix with row-major layout
// (each inner vector represents a row)
IntegerMatrix vec_to_matrix_rowmajor(const std::vector<std::vector<int>>& vec) {
  if (vec.empty()) {
    return IntegerMatrix(0, 0);
  }
  std::size_t nrow = vec.size();
  std::size_t ncol = vec[0].size();
  IntegerMatrix out(nrow, ncol);
  for (std::size_t i = 0; i < nrow; ++i) {
    for (std::size_t j = 0; j < ncol; ++j) {
      out(i, j) = vec[i][j];
    }
  }
  return out;
}
