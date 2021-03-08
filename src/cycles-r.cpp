#include "cpp11.hpp"

using namespace cpp11;
//namespace writable = cpp11::writable;

[[cpp11::register]]
double cycles_cpp(list df) {
    doubles d = df ["d"];
    doubles dw = df ["d_weighted"];
    const size_t n = d.size ();
    double z = 0.0;
    for (size_t i = 0; i < n; i++) {
        z += d [i] * dw [i];
    }

    return z;
}
