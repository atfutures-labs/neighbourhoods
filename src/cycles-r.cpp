#include "typedefs.h"
#include "preprocess.h"
#include "cycles.h"

#include "cpp11.hpp"
#include <unordered_set>

using namespace cpp11;

template <typename T1, typename T2>
void cycles_copy_column (
        const list &df,
        const std::string &col,
        std::vector <T2> &result)
{
    T1 s = df [col];
    result.resize (static_cast <size_t> (s.size ()));
    std::copy (s.begin (), s.end (), result.begin ());
}

[[cpp11::register]]
cpp11::writable::list cycles_cpp(list df, strings edge_list, const int start_edge_index, const bool left) {

    std::vector <std::string> edges;
    std::vector <std::string> v0;
    std::vector <std::string> v1;
    std::vector <double> x0;
    std::vector <double> y0;
    std::vector <double> x1;
    std::vector <double> y1;

    cycles_copy_column <strings, std::string> (df, "edge_", edges);
    cycles_copy_column <strings, std::string> (df, ".vx0", v0);
    cycles_copy_column <strings, std::string> (df, ".vx1", v1);
    cycles_copy_column <doubles, double> (df, ".vx0_x", x0);
    cycles_copy_column <doubles, double> (df, ".vx0_y", y0);
    cycles_copy_column <doubles, double> (df, ".vx1_x", x1);
    cycles_copy_column <doubles, double> (df, ".vx1_y", y1);

    Network network;
    cycles::fill_network (network, edges, v0, v1,
            x0, y0, x1, y1);

    PathData pathData;
    cycles::fillPathEdges (network, pathData);

    struct VecHash {

        size_t operator() (const std::vector<std::string>& v) const {

            std::hash <std::string> hasher;
            size_t seed = 0;

            for (std::string i : v) {
                seed ^= hasher(i) + 0x9e3779b9 + (seed<<6) + (seed>>2);
            }

            return seed;
        }
    };

    std::unordered_set <std::vector <std::string>, VecHash> path_edges;
    std::unordered_set <size_t> path_hashes;

    while (pathData.edgeList.size () > 1)
    {
        cycles::trace_cycle (network, pathData, left);

        size_t h = cycles::path_hash (pathData);
        if (path_hashes.count (h) == 0)
        {
            path_hashes.emplace (h);
            std::vector <std::string> edges_i;
            edges_i.reserve (pathData.path.size ());
            for (auto p: pathData.path)
                edges_i.push_back (p.edge);

            path_edges.emplace (edges_i);
        }
    }

    // return value is index into network edges obtained directly from edge_map
    cpp11::writable::list paths_out (static_cast <R_xlen_t> (path_edges.size ()));

    int i = 0;
    for (auto pe: path_edges)
    {
        cpp11::writable::integers edge_index (static_cast <R_xlen_t> (pe.size ()));
        int j = 0;
        for (auto p: pe) {
            edge_index [j++] = static_cast <int> (network.edge_map.at (p)) + 1L;
        }
        paths_out [i++] = edge_index;
    }
                
    return paths_out;
}
