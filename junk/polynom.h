#include <vector>
#include <cstddef>

inline double pow_int(double x, unsigned n) {
  double y = 1;
  for (unsigned i = 0; i < n; i++)
    y *= x;
  return y;
}

template <class Vec1, class Vec2>
std::vector<double> poly_calc(const Vec1& x, const Vec2& coefs) {
  std::size_t x_size = x.size();
  std::size_t coefs_size = coefs.size();
  std::vector<double> out(x_size);
  for (std::size_t i = 0; i != x_size; ++i) {
    for (std::size_t j = 0; j != coefs_size; ++j)
      out[i] += coefs[j] * pow_int(x[i], j);
  }
  return out;
}
