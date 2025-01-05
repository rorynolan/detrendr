#ifndef DETRENDR_SUMMARY_STATS_
#define DETRENDR_SUMMARY_STATS_

#include <vector>
#include <cmath>
#include <algorithm>
#include <numeric>
#include <functional>
#include <iterator>

#include <Rcpp.h>

template <class Iter>
double mymean(Iter start, Iter end) {
  if (start == end) return NAN;
  return std::accumulate(start, end, 0.0) / std::distance(start, end);
}

template <class Vec>
double mymean(const Vec& vec) {
  return mymean(vec.begin(), vec.end());
}

template <class Iter>
double myvar(Iter start, Iter end, double mean) {
  if (start == end) return NAN;
  double sum_sq = 0.0;
  for (auto it = start; it != end; ++it) {
    double diff = static_cast<double>(*it) - mean;
    sum_sq += diff * diff;
  }
  return sum_sq / (std::distance(start, end) - 1);
}

template <class Iter>
double myvar(Iter start, Iter end) {
  if (start == end) return NAN;
  double mean = mymean(start, end);
  return myvar(start, end, mean);
}

template <class Vec>
double myvar(const Vec& vec) {
  return myvar(vec.begin(), vec.end());
}

template <class Vec>
double myvar(const Vec& vec, const double mean) {
  return myvar(vec.begin(), vec.end(), mean);
}

template <class Iter>
double brightness(Iter start, Iter end, double mean) {
  if (start == end) return NAN;
  return myvar(start, end, mean) / mean;
}

template <class Iter>
double brightness(Iter start, Iter end) {
  if (start == end) return NAN;
  double mean = mymean(start, end);
  return myvar(start, end, mean) / mean;
}

template <class Vec>
double brightness(const Vec& vec) {
  return brightness(vec.begin(), vec.end());
}

template <class Vec>
double brightness(const Vec& vec, const double mean) {
  return brightness(vec.begin(), vec.end(), mean);
}

template <class T>
double mymedian(std::vector<T>& vec) {
  if (vec.size() == 0) return NAN;
  typedef typename std::vector<T>::size_type vec_sz;
  vec_sz n = vec.size() / 2;
  std::nth_element(vec.begin(), vec.begin() + n, vec.end());
  double med = vec[n];
  if(!(vec.size() & 1)) { //If the set size is even
    auto max_it = std::max_element(vec.begin(), vec.begin() + n);
    med = (*max_it + med) / 2.0;
  }
  return med;
}

template <class Vec>
int myprod(const Vec& vec) {
  return std::accumulate(vec.begin(), vec.end(), 1.0, std::multiplies<int>());
}

bool int_anyNA(Rcpp::IntegerVector x);
bool dbl_anyNA(Rcpp::NumericVector x);

// Declarations for std::vector overloads
bool int_anyNA(const std::vector<int>& x);
bool dbl_anyNA(const std::vector<double>& x);

#endif  // DETRENDR_SUMMARY_STATS_
