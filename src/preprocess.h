#pragma once

#include "typedefs.h"

#include "cpp11.hpp"

#include <limits>
#include <numeric> // std::iota
#include <iostream> // TODO: Remove that
#include <ctime>
#include <stdexcept>
#include <string>
#include <functional> // hash
#include <vector>
#include <unordered_set>
#include <unordered_map>

using namespace cpp11;

struct node_hash {
    inline std::size_t operator() (const node_t & n) const {
        std::hash <node_t> node_hasher;
        return node_hasher (n);
    }
};

namespace utils {

using namespace cpp11;

template <typename T1, typename T2>
void copy_column (
        const list &df,
        const std::string &col,
        std::vector <T2> &result);

template <typename T>
void copy_vec (
        const std::vector <T> &vfrom,
        std::vector <T> &vto);

} // end namespace utils

namespace preprocess {

size_t node_hash (const node_t n);

const nodeset_t get_terminal_nodes (
        const std::vector <node_t> &n1,
        const std::vector <node_t> &n2);

void get_one_terminal_nodes (
        const std::vector <node_t> &nodes,
        nodeset_t &terminal,
        nodeset_t &not_terminal);

} // end namespace preprocess
