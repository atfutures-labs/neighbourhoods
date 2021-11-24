#include "expand_edges.h"
#include <unordered_map>

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
void cpp_expand_edges(const list edges, const list edge_map_in) {


    std::vector <std::string> edge_old, edge_new;

    edges_copy_column <strings, std::string> (edge_map_in, "edge_old", edge_old);
    edges_copy_column <strings, std::string> (edge_map_in, "edge_new", edge_new);

    std::unordered_map <std::string, std::set <std::string> > edge_map;

    const size_t n = edge_old.size ();
    for (size_t i = 0; i < n; i++) {

        std::set <std::string> edge_set;
        if (edge_map.find (edge_new [i]) != edge_map.end ()) {
            edge_set = edge_map.at (edge_new [i]);
            edge_map.erase (edge_new [i]);
        }
        edge_set.insert (edge_old [i]);

        edge_map.emplace (edge_new [i], edge_set);
    }

    for (const list pi: edges) {

        std::vector <std::string> edges_i;
        edges_copy_column <strings, std::string> (pi, "edge_", edges_i);
    }
}
