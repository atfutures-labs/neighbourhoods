#include "cycles.h"
#include <stdexcept>
#include <unordered_set>

void build_network::fill_network (Network &network,
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

void build_network::fillPathEdges (const Network &network,
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

bool cycles::increment_cycle (const Network &network,
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
    if (edge_i == INFINITE_INT)
        return false;

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
            for (size_t i = 0; i < nbs.size (); i++)
            {
                nbs_x [i] = network.edges [nbs [i]].x1;
                nbs_y [i] = network.edges [nbs [i]].y1;
            }
            size_t lefty = clockwise::to_left (
                    this_edge.x0,
                    this_edge.y0,
                    this_edge.x1,
                    this_edge.y1,
                    nbs_x, nbs_y, left);
            pathData.left_nb = nbs [lefty];
        }
    }

    return true;
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
    pathData.path.clear ();

    bool check = false;
    while (!check)
    {
        std::string nextEdge = cycles::nextPathEdge (pathData);
        check = cycles::increment_cycle (network, pathData, nextEdge, left, true);
    }

    size_t loop_vert = INFINITE_INT;
    while (loop_vert == INFINITE_INT)
    {
        check = false;
        while (!check)
        {
            check = cycles::increment_cycle (network, pathData, "", left, false);

            if (!check)
                check = (pathData.edgeList.size () == 0);
        }
        loop_vert = cycles::path_loop_vert (pathData);
        if (pathData.edgeList.size () == 0)
            loop_vert = 0;
    }

    // remove path edges from startEdge candidates:
    for (auto p: pathData.path)
    {
        if (pathData.edgeList.find (p.edge) !=
                pathData.edgeList.end ())
            pathData.edgeList.erase (p.edge);
    }

    cycles::cut_path (pathData);
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

    if (loop_vert != INFINITE_INT)
    {
        for (size_t i = 0; i < loop_vert; i++)
            pathData.path.erase (pathData.path.begin ());
    }
}


size_t cycles::path_hash (const PathData &pathData)
{
    // first get the ordered edge_set
    std::set <std::string> edge_set;
    for (auto p: pathData.path)
    {
        std::string this_edge = p.edge;
        utils::cut_terminal_rev (this_edge);
        edge_set.emplace (this_edge);
    }

    // then construct the hash from the full set
    std::hash <std::string> hasher;

    size_t h = 0; // the hash
    for (auto e: edge_set)
    {
        h ^= hasher (e) + 0x9e3779b9 + (h<<6) + (h>>2);
    }

    return h;
}

void cycles::trace_edge_set (PathData &pathData, PathEdgeSet &path_edges,
        const Network &network, const bool left,
        std::unordered_set <size_t> &path_hashes)
{

    while (pathData.edgeList.size () > 1)
    {
        cycles::trace_cycle (network, pathData, left);

        size_t h = cycles::path_hash (pathData);
        if (path_hashes.count (h) == 0L)
        {
            path_hashes.emplace (h);
            std::vector <std::string> edges_i;
            edges_i.reserve (pathData.path.size ());
            for (auto p: pathData.path)
                edges_i.push_back (p.edge);

            path_edges.emplace (edges_i);
        }
    }
}

void next_cycle::single_edges (const PathEdgeSet &path_edges, PathData &pathData)
{

    std::unordered_map <std::string, size_t> edge_count;

    for (auto path: path_edges)
    {
        for (auto p: path)
        {
            std::string this_edge = p;
            utils::cut_terminal_rev (this_edge);

            size_t this_count = 0L;
            if (edge_count.count (this_edge) > 0)
            {
                this_count = edge_count.at (this_edge);
                this_count++;
                edge_count.erase (this_edge);
            }
            edge_count.emplace (this_edge, this_count);
        }
    }

    pathData.edgeList.clear ();
    for (auto e: edge_count)
    {
        if (e.second == 0L)
        {
            pathData.edgeList.emplace (e.first);
            pathData.edgeList.emplace (e.first + "_rev");
        }
    }

}
