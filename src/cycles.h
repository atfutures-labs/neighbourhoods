#pragma once

#include "cpp11.hpp"

#include <limits>
#include <string>
#include <unordered_set>
#include <unordered_map>

#include "clockwise.h"

const int INFINITE_INT = std::numeric_limits <int>::max ();

struct OneEdge
{
    double x0, y0, x1, y1;
    std::string v0;
    std::string v1;
    std::string edge;
};

struct Network
{
    std::vector <OneEdge> edges;
    // map from edge ID to network index
    std::unordered_map <std::string, size_t> edge_map;
    // maps from vertex IDs to network indices of all edges which start/end at
    // that vertex:
    std::unordered_map <std::string, std::vector <size_t> > v0_map;
    std::unordered_map <std::string, std::vector <size_t> > v1_map;
};


struct PathData
{
    std::vector <OneEdge> path;
    int left_nb;
};

namespace cycles {

void fill_network (Network &network,
        const std::vector <std::string> edges,
        const std::vector <std::string> v0,
        const std::vector <std::string> v1,
        const std::vector <double> x0,
        const std::vector <double> y0,
        const std::vector <double> x1,
        const std::vector <double> y1);

std::vector <size_t> get_nbs (const Network &network,
        const size_t this_edge);

void increment_cycle (const Network &network,
        PathData &pathData,
        const std::string &start_edge,
        const bool left = true,
        const bool start = true);

size_t path_loop_vert (const PathData &pathData);

} // end namespace cycles
