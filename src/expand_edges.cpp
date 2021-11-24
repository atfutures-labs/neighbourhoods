#include "expand_edges.h"

using namespace cpp11;
namespace writable = cpp11::writable;

template <typename T1, typename T2>
void edges_copy_column (
        const list &df,
        const std::string &col,
        std::vector <T2> &result)
{
    T1 s = df [col];
    result.resize (static_cast <size_t> (s.size ()));
    std::copy (s.begin (), s.end (), result.begin ());
}


[[cpp11::register]]
void cpp_expand_edges(const list edges, const list edge_map) {


    std::vector <std::string> edge_old, edge_new;

    edges_copy_column <strings, std::string> (edge_map, "edge_old", edge_old);
    edges_copy_column <strings, std::string> (edge_map, "edge_new", edge_new);
}
