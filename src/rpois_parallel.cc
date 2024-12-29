// [[Rcpp::depends(RcppThread)]]

#include <random>
#include <vector>
#include <Rcpp.h>
#include <RcppThread.h>

using namespace Rcpp;

// Forward declarations from utils.cpp
IntegerMatrix vec_to_matrix_colmajor(const std::vector<std::vector<int>>& vec);
IntegerMatrix vec_to_matrix_rowmajor(const std::vector<std::vector<int>>& vec);

int mysign(double x) {
  if (x >= 0) {
    return 1;
  } else {
    return -1;
  }
}

// [[Rcpp::export]]
IntegerVector myrpois_(NumericVector means, int seed) {
  std::size_t n = means.size();
  if (n == 0) {
    return IntegerVector(0);
  }
  
  std::vector<int> temp_output(n);
  
  auto worker = [&](std::size_t i) {
    std::minstd_rand generator_int(seed + i);
    std::uniform_int_distribution<int> distribution_int(1, RAND_MAX);
    int seed_i = distribution_int(generator_int);
    std::minstd_rand generator(seed_i);
    std::poisson_distribution<int> distribution(std::abs(means[i]));
    temp_output[i] = distribution(generator) * mysign(means[i]);
  };

  RcppThread::parallelFor(0, n, worker);
  return IntegerVector(temp_output.begin(), temp_output.end());
}

// [[Rcpp::export]]
IntegerMatrix myrpois_frames_(NumericVector means, std::size_t frame_length,
                             int seed) {
  std::size_t ncol = means.size();
  if (ncol == 0) {
    return IntegerMatrix(frame_length, 0);
  }
  
  std::vector<std::vector<int>> temp_output(ncol, std::vector<int>(frame_length));
  
  auto worker = [&](std::size_t i) {
    std::minstd_rand generator_int(seed + i);
    std::uniform_int_distribution<int> distribution_int(1, RAND_MAX);
    int seed_i = distribution_int(generator_int);
    std::minstd_rand generator(seed_i);
    std::poisson_distribution<int> distribution(std::abs(means[i]));
    for (std::size_t j = 0; j != frame_length; ++j) {
      temp_output[i][j] = distribution(generator) * mysign(means[i]);
    }
  };

  RcppThread::parallelFor(0, ncol, worker);
  return vec_to_matrix_colmajor(temp_output);
}

// [[Rcpp::export]]
IntegerMatrix myrpois_frames_t_(NumericVector means, std::size_t frame_length,
                               int seed) {
  std::size_t nrow = means.size();
  if (nrow == 0) {
    return IntegerMatrix(0, frame_length);
  }
  
  std::vector<std::vector<int>> temp_output(nrow, std::vector<int>(frame_length));
  
  auto worker = [&](std::size_t i) {
    std::minstd_rand generator_int(seed + i);
    std::uniform_int_distribution<int> distribution_int(1, RAND_MAX);
    int seed_i = distribution_int(generator_int);
    std::minstd_rand generator(seed_i);
    std::poisson_distribution<int> distribution(std::abs(means[i]));
    for (std::size_t j = 0; j != frame_length; ++j) {
      temp_output[i][j] = distribution(generator) * mysign(means[i]);
    }
  };

  RcppThread::parallelFor(0, nrow, worker);
  return vec_to_matrix_rowmajor(temp_output);
}
