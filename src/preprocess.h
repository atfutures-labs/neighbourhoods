#pragma once

#include "cpp11.hpp"

#include <limits>
#include <numeric> // std::iota
#include <iostream> // TODO: Remove that
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

template <typename T1, typename T2>
void copy_column (
        const list &df,
        const std::string &col,
        std::vector <T2> &result);

template <typename T>
void copy_vec (
        const std::vector <T> &vfrom,
        std::vector <T> &vto);

const nodeset_t get_terminal_nodes (
        const std::vector <std::string> &n1,
        const std::vector <std::string> &n2);

void get_one_terminal_nodes (
        const std::vector <std::string> &nodes,
        nodeset_t &terminal,
        nodeset_t &not_terminal);

} // end namespace preprocess
