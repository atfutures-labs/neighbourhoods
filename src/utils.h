#pragma once

#include "typedefs.h"
#include <cstring> // strcmp
#include <algorithm> // sort
#include <numeric> // iota

namespace utils {

void cut_terminal_rev (std::string &s);

template <typename T>
std::vector<size_t> sort_indexes(const std::vector<T> &v);

}
