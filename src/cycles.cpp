#include "cycles.h"
#include <stdexcept>

void cycles::fill_network (Network &network,
        const std::vector <std::string> edges,
        const std::vector <std::string> v0,
        const std::vector <std::string> v1,
        const std::vector <double> x0,
        const std::vector <double> y0,
        const std::vector <double> x1,
        const std::vector <double> y1)
{

    const size_t n = edges.size ();

    network.edges.resize (n);

    for (size_t i = 0; i < n; i++)
    {

        network.edges [i].edge = edges [i];
        network.edges [i].v0 = v0 [i];
        network.edges [i].v1 = v1 [i];

        network.edges [i].x0 = x0 [i];
        network.edges [i].y0 = y0 [i];
        network.edges [i].x1 = x1 [i];
        network.edges [i].y1 = y1 [i];

        network.edge_map.emplace (edges [i], i);

        std::vector <size_t> v0_vec;
        if (network.v0_map.count (v0 [i]) > 0)
        {
            v0_vec = network.v0_map.at (v0 [i]);
            network.v0_map.erase (v0 [i]);
        }
        v0_vec.push_back (i);
        network.v0_map.emplace (v0 [i], v0_vec);

        std::vector <size_t> v1_vec;
        if (network.v1_map.count (v1 [i]) > 0)
        {
            v1_vec = network.v1_map.at (v1 [i]);
            network.v1_map.erase (v1 [i]);
        }
        v1_vec.push_back (i);
        network.v1_map.emplace (v1 [i], v1_vec);
    } // end for i
}

void cycles::fillPathEdges (const Network &network,
        PathData &pathData)
{
    pathData.edgeList.clear ();
    for (auto e: network.edges)
        pathData.edgeList.emplace (e.edge);
}

std::string cycles::nextPathEdge (PathData &pathData)
{
    auto nextEdgeItr = pathData.edgeList.begin ();
    std::string nextEdge = *nextEdgeItr;
    pathData.edgeList.erase (nextEdgeItr);

    return nextEdge;
}

std::vector <size_t> cycles::get_nbs (const Network &network,
        const size_t this_edge)
{
    const std::string v0 = network.edges [this_edge].v0;
    const std::string v1 = network.edges [this_edge].v1;

    // Get all edges which have v0 == this_edge.v1:
    const std::vector <size_t> v1_nbs = network.v0_map.at (v1); 

    bool v1_has_v0 = false;
    for (auto i: v1_nbs)
    {
        if (network.edges [i].v1 == v0)
        {
            v1_has_v0 = true;
            break;
        }
    }

    size_t nnbs = v1_nbs.size ();
    // plus remove reverse edges which point back to same v0
    if (v1_has_v0)
        nnbs--;


    std::vector <size_t> nbs (nnbs);

    if (nnbs == 0)
        return nbs;

    size_t count = 0;
    for (auto i: v1_nbs)
    {
        if (network.edges [i].v1 != v0)
            nbs [count++] = i;
    }

    return nbs;
}

void cycles::increment_cycle (const Network &network,
        PathData &pathData,
        const std::string &start_edge,
        const bool left,
        const bool start)
{
    size_t edge_i;

    if (start)
    {
        if (network.edge_map.find (start_edge) ==
                network.edge_map.end ())
            cpp11::stop ("edge not found");

        edge_i = network.edge_map.at (start_edge);
    } else
    {
        edge_i = pathData.left_nb;
    }
    if (pathData.left_nb == INFINITE_INT)
        return; // TODO: That does not increment properly by removing that edge

    OneEdge this_edge = network.edges [static_cast <size_t> (edge_i)];
    pathData.path.push_back (this_edge);

    std::vector <size_t> nbs = cycles::get_nbs (network, edge_i);
    pathData.left_nb = INFINITE_INT;

    if (nbs.size () > 0)
    {
        if (nbs.size () == 1)
        {
            pathData.left_nb = nbs [0];
        } else
        {
            std::vector <double> nbs_x (nbs.size ());
            std::vector <double> nbs_y (nbs.size ());
            size_t lefty = clockwise::to_left (
                    this_edge.x0,
                    this_edge.y0,
                    this_edge.x1,
                    this_edge.y1,
                    nbs_x, nbs_y, left);
            pathData.left_nb = nbs [lefty];
        }
    }
}

//' Determine the index where the path connects back on itself.
size_t cycles::path_loop_vert (const PathData &pathData)
{
    std::unordered_set <std::string> pathVerts;

    size_t loop_vert = INFINITE_INT;

    size_t count = 0;
    for (auto p: pathData.path)
    {
        pathVerts.emplace (p.v0);
        if (pathVerts.find (p.v1) != pathVerts.end ())
        {
            loop_vert = count;
            break;
        }
        count++;
    }

    return loop_vert;
}

void cycles::trace_cycle (const Network &network,
        PathData &pathData,
        const bool left)
{
    std::string nextEdge = cycles::nextPathEdge (pathData);
    cycles::increment_cycle (network, pathData, nextEdge, left, true);

    size_t loop_vert = INFINITE_INT;
    while (loop_vert == INFINITE_INT)
    {
        nextEdge = cycles::nextPathEdge (pathData);
        cycles::increment_cycle (network, pathData, nextEdge, left, false);
        loop_vert = cycles::path_loop_vert (pathData);
    }

    // remove path edges from startEdge candidates:
    for (auto p: pathData.path)
    {
        if (pathData.edgeList.find (p.edge) !=
                pathData.edgeList.end ())
            pathData.edgeList.erase (p.edge);
    }
}

void cycles::cut_path (PathData &pathData)
{
    std::string lastVert = pathData.path.back ().v1;

    size_t loop_vert = INFINITE_INT;

    size_t count = 0;
    for (auto p: pathData.path)
    {
        if (p.v0 == lastVert)
        {
            loop_vert = count;
            break;
        }
        count++;
    }

    if (loop_vert == INFINITE_INT)
        cpp11::stop ("path does not enclose a polygon");

    for (size_t i = 0; i < loop_vert; i++)
        pathData.path.erase (pathData.path.begin ());
}
