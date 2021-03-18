#pragma once

#include <limits>
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>

const int INFINITE_INT = std::numeric_limits <int>::max ();

typedef std::string node_t;

typedef std::unordered_map <node_t, int> nodemap_t;
typedef std::unordered_set <node_t> nodeset_t;
