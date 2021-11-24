#pragma once

#include "typedefs.h"

#include "cpp11.hpp"

#include <limits>
#include <numeric> // std::iota
#include <iostream> // TODO: Remove that
#include <ctime>
#include <stdexcept>
#include <string>
#include <vector>
#include <unordered_map>
#include <set>

typedef std::unordered_map <std::string, std::set <std::string> > EdgeMapType;

using namespace cpp11;

namespace expand_edges {

inline size_t count_edges (
        const EdgeMapType &edge_map,
        const std::vector <std::string> &edges);

} // end namespace expand_edges
