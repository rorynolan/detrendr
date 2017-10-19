#ifndef DETRENDR_SMOOTH_
#define DETRENDR_SMOOTH_

#include <stdexcept>
#include <algorithm>
#include <numeric>

#include <Rcpp.h>

#include "summary_stats.h"

using namespace Rcpp;

template <class ReturnVec, class Vec>
ReturnVec boxcar_smooth(const Vec& extended,
                        const std::size_t extended_both_sides_by,
                        const std::size_t l) {
  const std::size_t extended_size = extended.size();
  const std::size_t smoothed_size = extended_size -
    2 * extended_both_sides_by;
  ReturnVec smoothed(smoothed_size);
  typename ReturnVec::const_iterator orig_start = extended.begin() +
    extended_both_sides_by;
  for (std::size_t i = 0; i != smoothed_size; ++i) {
    std::size_t go_left = std::min(l, extended_both_sides_by + i);
    std::size_t go_right = std::min(l + 1, extended_size -
                                             (extended_both_sides_by + i));
    smoothed[i] = mymean(orig_start + i - go_left, orig_start + i + go_right);
  }
  return smoothed;
}


template <class ReturnVec, class Vec>
ReturnVec weighted_smooth(const Vec& extended,
                          const std::size_t extended_both_sides_by,
                          const Vec& weights, double weight_sum) {
  std::size_t weights_size = weights.size();
  std::size_t extended_size = extended.size();
  if (weights_size % 2 == 0)
    throw std::domain_error("The number of elements in weights must be odd.");
  const std::size_t l = weights_size / 2;
  const std::size_t smoothed_size = extended_size - 2 * extended_both_sides_by;
  ReturnVec smoothed(smoothed_size);
  if (extended_both_sides_by >= l) {
    typename Vec::const_iterator extended_start = extended.begin() +
      (extended_both_sides_by - l);
    for (std::size_t i = 0; i != smoothed_size; ++i) {
      smoothed[i] = std::inner_product(weights.begin(), weights.end(),
                                       extended_start + i, 0.0) / weight_sum;
    }
  } else {
    typename Vec::const_iterator orig_start = extended.begin() +
      extended_both_sides_by;
    for (std::size_t i = 0; i != smoothed_size; ++i) {
      std::size_t clip_left = std::max(std::intmax_t(0),
                                       std::intmax_t(l) - std::intmax_t(i));
      std::size_t clip_right = std::max(std::intmax_t(0),
                                        std::intmax_t(l) -
                                          std::intmax_t(smoothed_size - i - 1));
      smoothed[i] = std::inner_product(
                      weights.begin() + clip_left, weights.end() - clip_right,
                      orig_start + (i - l + clip_left), 0.0);
      weight_sum = std::accumulate(weights.begin() + clip_left,
                                   weights.end() - clip_right, 0.0);
      smoothed[i] /= weight_sum;
    }
  }
  return smoothed;
}


template <class ReturnVec, class Vec>
ReturnVec exp_smooth(Vec extended,
                     const std::size_t extended_both_sides_by,
                     const double tau, const std::size_t l) {
  Vec weights(2 * l + 1);
  weights[l] = 1;
  for (int i = 1; i != l + 1; ++i) {
    weights[l - i] = weights[l + i] = std::exp(- i / tau);
  }
  return weighted_smooth<ReturnVec>(extended, extended_both_sides_by, weights,
                                    std::accumulate(weights.begin(),
                                                    weights.end(), 0.0));
}


#endif  // DETRENDR_SMOOTH_
