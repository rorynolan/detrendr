#ifndef DETRENDR_MED_REFLECT_
#define DETRENDR_MED_REFLECT_

#include <vector>
#include <algorithm>
#include <stdexcept>
#include <cmath>
#include <numeric>

#include "summary_stats.h"

template <class Vec>
double reflect_index_med(const Vec& vec, std::size_t ind, char side) {
  int n = vec.size();
  double out = NAN;
  double med;
  int dist_to_end;
  int dist_to_go;
  double median_dist;
  if (ind < n) {
    if (side == 'l') {
      std::vector<double> left(ind);
      for (int i = 0; i < ind; i++) {
        left[i] = vec[i];
      }
      med = mymedian(left);
      dist_to_end = ind;
      dist_to_go = 2 * dist_to_end;
      median_dist = 1 + 0.5 * (dist_to_end - 1);
      out = vec[ind] + dist_to_go / median_dist * (med - vec[ind]);
    } else if (side == 'r') {
      std::vector<double> right(n - (ind + 1));
      for (int i = 0; i < n - (ind + 1); i++) {
        right[i] = vec[ind + i + 1];
      }
      med = mymedian(right);
      dist_to_end = (n - 1) - ind;
      dist_to_go = 2 * dist_to_end;
      median_dist = 1 + 0.5 * (dist_to_end - 1);
      out = vec[ind] + dist_to_go / median_dist * (med - vec[ind]);
    }
  }
  return out;
}

template <class Vec>
std::vector<double> mysmooth(const Vec& vec) {
  std::size_t n = vec.size();
  std::vector<double> smoothed(n);
  if (n > 1) {
    smoothed[0] = (2 * vec[0] + vec[1]) / 3;
    smoothed[n - 1] = (vec[n - 2] + 2 * vec[n - 1]) / 3;
    for (std::size_t i = 1; i != vec.size() - 1; ++i) {
      smoothed[i] = std::accumulate(vec.begin() + (i - 1),
                                    vec.begin() + (i + 2), 0.0) / 3;
    }
  } else {
    smoothed[0] = vec[0];
  }
  return smoothed;
}

template <class Vec>
std::vector<double> med_reflect_extend_(const Vec& vec, const Vec& orig,
                                        std::size_t extend_both_sides_by,
                                        bool preserve_mean = false,
                                        bool smooth = false) {
  std::size_t vec_size = vec.size();
  std::size_t orig_size = orig.size();
  if (orig_size > vec_size)
    throw std::domain_error("orig_size cannot be greater than size of vec");
  std::size_t max_extended_size = 3 * orig_size - 2;
  if (vec_size + 2 * extend_both_sides_by > max_extended_size)
    throw std::domain_error("extend_both_sides_by is too big; "
                              "maximum end size is "
                              "three times original size minus two.");
  const std::size_t extended_size = vec_size + 2 * extend_both_sides_by;
  std::vector<double> extended(extended_size);
  std::copy(vec.begin(), vec.end(), extended.begin() + extend_both_sides_by);
  std::size_t already_extended_total = vec_size - orig_size;
  if (already_extended_total % 2 != 0)
    throw std::domain_error("The difference between the size of vec and "
                              "the size of orig should be an even number.");
  std::size_t already_extended_both_sides =
    already_extended_total / 2;
  for (std::size_t i = 1; i <= extend_both_sides_by; ++i) {
    extended[extend_both_sides_by - i] =
      reflect_index_med(orig, already_extended_both_sides + i , 'l');
    extended[extended_size - extend_both_sides_by - 1 + i] =
      reflect_index_med(orig,
                        orig_size - (already_extended_both_sides + i + 1), 'r');
  }
  if (smooth) {
    std::vector<double> extended_side(extended.begin(),
                                      extended.begin() + extend_both_sides_by);
    extended_side = mysmooth(extended_side);
    std::copy(extended_side.begin(), extended_side.end(), extended.begin());
    std::copy(extended.end() - extend_both_sides_by, extended.end(),
              extended_side.begin());
    extended_side = mysmooth(extended_side);
    std::copy(extended_side.begin(), extended_side.end(),
              extended.end() - extend_both_sides_by);
  }
  if (preserve_mean) {
    int atoms_to_add = extend_both_sides_by * (extend_both_sides_by + 1);
    double to_add = (mymean(orig) - mymean(extended)) * extended_size;
    if (to_add != 0) {
      double atom = to_add / atoms_to_add;
      for (std::vector<double>::size_type i = 0;
           i != extend_both_sides_by; ++i) {
        double this_add = (i + 1) * atom;
        extended[extend_both_sides_by - i - 1] += this_add;
        extended[extended_size - extend_both_sides_by + i] += this_add;
      }
    }
  }
  return extended;
}

#endif  // DETRENDR_MED_REFLECT_
