// [[Rcpp::depends(RcppThread)]]

#include <random>
#include <vector>
#include <Rcpp.h>
#include <RcppThread.h>

using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector myrbernoulli_(NumericVector p, int seed) {
  std::size_t n = p.size();
  if (n == 0) {
    return IntegerVector(0);
  }
  
  // Check probability bounds
  for (std::size_t i = 0; i < n; ++i) {
    if (p[i] < 0 || p[i] > 1) {
      stop("All probability values must be between 0 and 1");
    }
  }
  
  std::vector<int> temp_output(n);
  
  auto worker = [&](std::size_t i) {
    std::minstd_rand generator_int(seed + i);
    std::uniform_int_distribution<int> distribution_int(1, RAND_MAX);
    int seed_i = distribution_int(generator_int);
    std::minstd_rand generator(seed_i);
    std::bernoulli_distribution distribution(p[i]);
    temp_output[i] = distribution(generator);
  };

  RcppThread::parallelFor(0, n, worker);
  return IntegerVector(temp_output.begin(), temp_output.end());
}
