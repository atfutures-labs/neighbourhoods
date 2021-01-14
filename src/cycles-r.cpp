#include "cpp11.hpp"

using namespace cpp11;
namespace writable = cpp11::writable;

[[cpp11::register]]
double test_cpp(list df) {
    doubles x = df ["x"];
    doubles y = df ["y"];
    const size_t n = x.size ();
    double z = 0.0;
    for (size_t i = 0; i < n; i++) {
        z += x [i] * y [i];
    }

    return z;
}

[[cpp11::register]]
double cycles_cpp() {
    return 1.0;
}
