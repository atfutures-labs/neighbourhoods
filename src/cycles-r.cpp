#include "typedefs.h"
#include "preprocess.h"
#include "cycles.h"

#include "cpp11.hpp"

using namespace cpp11;
//namespace writable = cpp11::writable;

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
int cycles_cpp(list df, strings edge_list, const int start_edge_index, const bool left) {

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

    std::string start_edge = edge_list [start_edge_index - 1];

    cycles::trace_cycle (network, pathData, left);

    return static_cast <int> (pathData.path.size ());
}
