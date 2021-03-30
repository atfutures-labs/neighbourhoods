#pragma once

#include "typedefs.h"

#include "cpp11.hpp"

#include <set>
#include <iostream> // TODO: Remove that
#include <algorithm> // sort
#include <cstring> // strcmp

#include "clockwise.h"

struct OneEdge
{
    double x0, y0, x1, y1;
    std::string v0;
    std::string v1;
    std::string edge;
};

typedef std::vector <OneEdge> EdgeVec;

struct Network
{
    EdgeVec edges;
    // map from edge ID to network index
    std::unordered_map <std::string, size_t> edge_map;
    // maps from vertex IDs to network indices of all edges which start/end at
    // that vertex:
    std::unordered_map <std::string, std::vector <size_t> > v0_map;
    std::unordered_map <std::string, std::vector <size_t> > v1_map;
};


struct PathData
{
    std::set <std::string> edgeList; // list of edges to trace
    EdgeVec path;
    size_t left_nb;
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

void fillPathEdges (const Network &network,
        PathData &pathData);

std::string nextPathEdge (PathData &pathData);

std::vector <size_t> get_nbs (const Network &network,
        const size_t this_edge);

bool increment_cycle (const Network &network,
        PathData &pathData,
        const std::string &start_edge,
        const bool left = true,
        const bool start = true);

size_t path_loop_vert (const PathData &pathData);

void trace_cycle (const Network &network,
        PathData &pathData,
        const bool left = true);

void cut_path (PathData &pathData);

size_t path_hash (const PathData &pathData);

} // end namespace cycles
