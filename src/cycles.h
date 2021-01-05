#pragma once

#include <Rcpp.h>

struct OneEdge
{
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

struct OnePath
{
    std::vector <OneEdge> path;
};

struct PathData
{
    OnePath path;
    std::string left_nb;
};

namespace cycles {

void fill_network (Network &network,
        const std::vector <std::string> edges,
        const std::vector <std::string> v0,
        const std::vector <std::string> v1);

std::vector <size_t> get_nbs (const Network &network,
        const size_t this_edge);

void start_next_cycle (const Network &network,
        PathData &pathData,
        const std::string &start_edge);

//std::vector <std::string> get_nbs (

} // end namespace cycles
