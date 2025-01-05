#include <numeric>

#include <Rcpp.h>
#include <RcppThread.h>

#include "summary_stats.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector mean_frames_(NumericVector arr3d) {
  IntegerVector arr3d_dim = arr3d.attr("dim");
  NumericVector output(arr3d_dim[2]);
  std::size_t frame_length = arr3d_dim[0] * arr3d_dim[1];

  RcppThread::parallelFor(0, arr3d_dim[2], [&](std::size_t frame) {
    output[frame] = mymean(arr3d.begin() + (frame_length * frame),
                          arr3d.begin() + (frame_length * (frame + 1)));
  });

  return output;
}

// [[Rcpp::export]]
NumericVector sum_frames_(NumericVector arr3d) {
  IntegerVector arr3d_dim = arr3d.attr("dim");
  NumericVector output(arr3d_dim[2]);
  std::size_t frame_length = arr3d_dim[0] * arr3d_dim[1];

  RcppThread::parallelFor(0, arr3d_dim[2], [&](std::size_t frame) {
    output[frame] = std::accumulate(arr3d.begin() + (frame_length * frame),
                                  arr3d.begin() + (frame_length * (frame + 1)),
                                  0.0);
  });

  return output;
}

double sum_na_omit(NumericVector x) {
  NumericVector x_noNA = na_omit(x);
  if (x_noNA.size() > 0) {
    return sum(x_noNA);
  } else {
    return NA_REAL;
  }
}

double mean_na_omit(NumericVector x) {
  NumericVector x_noNA = wrap(na_omit(x));
  if (x_noNA.size() > 0) {
    return mean(x_noNA);
  } else {
    return NA_REAL;
  }
}

double mean_na_omit(IntegerVector x) {
  IntegerVector x_noNA = wrap(na_omit(x));
  if (x_noNA.size() > 0) {
    return mean(x_noNA);
  } else {
    return NA_REAL;
  }
}

template<typename T>
bool is_na(T x) {
  return Rcpp::traits::is_na<REALSXP>(x);
}

template<>
bool is_na(int x) {
  return x == NA_INTEGER;
}

template<typename T>
double compute_frame_sum_na_omit(const T* begin, const T* end) {
  std::vector<double> noNA;
  noNA.reserve(std::distance(begin, end));
  
  for (const T* it = begin; it != end; ++it) {
    if (!is_na(*it)) {
      noNA.push_back(static_cast<double>(*it));
    }
  }
  
  if (noNA.empty()) {
    return NA_REAL;
  }
  return std::accumulate(noNA.begin(), noNA.end(), 0.0);
}

template<typename T>
double compute_frame_mean_na_omit(const T* begin, const T* end) {
  std::vector<double> noNA;
  noNA.reserve(std::distance(begin, end));
  
  for (const T* it = begin; it != end; ++it) {
    if (!is_na(*it)) {
      noNA.push_back(static_cast<double>(*it));
    }
  }
  
  if (noNA.empty()) {
    return NA_REAL;
  }
  return std::accumulate(noNA.begin(), noNA.end(), 0.0) / noNA.size();
}

// [[Rcpp::export]]
NumericVector int_sum_frames_na_omit(IntegerVector arr3d) {
  IntegerVector arr3d_dim = arr3d.attr("dim");
  NumericVector output(arr3d_dim[2]);
  std::size_t frame_length = arr3d_dim[0] * arr3d_dim[1];

  RcppThread::parallelFor(0, arr3d_dim[2], [&](std::size_t frame) {
    const int* frame_begin = arr3d.begin() + (frame_length * frame);
    const int* frame_end = arr3d.begin() + (frame_length * (frame + 1));
    output[frame] = compute_frame_sum_na_omit(frame_begin, frame_end);
  });

  return output;
}

// [[Rcpp::export]]
NumericVector dbl_sum_frames_na_omit(NumericVector arr3d) {
  IntegerVector arr3d_dim = arr3d.attr("dim");
  NumericVector output(arr3d_dim[2]);
  std::size_t frame_length = arr3d_dim[0] * arr3d_dim[1];

  RcppThread::parallelFor(0, arr3d_dim[2], [&](std::size_t frame) {
    const double* frame_begin = arr3d.begin() + (frame_length * frame);
    const double* frame_end = arr3d.begin() + (frame_length * (frame + 1));
    output[frame] = compute_frame_sum_na_omit(frame_begin, frame_end);
  });

  return output;
}

// [[Rcpp::export]]
NumericVector int_mean_frames_na_omit(IntegerVector arr3d) {
  IntegerVector arr3d_dim = arr3d.attr("dim");
  NumericVector output(arr3d_dim[2]);
  std::size_t frame_length = arr3d_dim[0] * arr3d_dim[1];

  RcppThread::parallelFor(0, arr3d_dim[2], [&](std::size_t frame) {
    const int* frame_begin = arr3d.begin() + (frame_length * frame);
    const int* frame_end = arr3d.begin() + (frame_length * (frame + 1));
    output[frame] = compute_frame_mean_na_omit(frame_begin, frame_end);
  });

  return output;
}

// [[Rcpp::export]]
NumericVector dbl_mean_frames_na_omit(NumericVector arr3d) {
  IntegerVector arr3d_dim = arr3d.attr("dim");
  NumericVector output(arr3d_dim[2]);
  std::size_t frame_length = arr3d_dim[0] * arr3d_dim[1];

  RcppThread::parallelFor(0, arr3d_dim[2], [&](std::size_t frame) {
    const double* frame_begin = arr3d.begin() + (frame_length * frame);
    const double* frame_end = arr3d.begin() + (frame_length * (frame + 1));
    output[frame] = compute_frame_mean_na_omit(frame_begin, frame_end);
  });

  return output;
}
