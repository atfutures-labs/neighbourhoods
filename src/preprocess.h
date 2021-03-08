#pragma once

#include "cpp11.hpp"

#include <limits>
#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>

using namespace cpp11;

const int INFINITE_INT = std::numeric_limits <int>::max ();

typedef std::string node_t;
typedef std::unordered_map <node_t, int> nodemap_t;
typedef std::unordered_set <node_t> nodeset_t;

namespace preprocess {

    const nodeset_t get_terminal_nodes (
            const std::vector <node_t> &n1,
            const std::vector <node_t> &n2);
} // end namespace preprocess
