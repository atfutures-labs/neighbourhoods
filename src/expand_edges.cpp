#include "expand_edges.h"
#include <unordered_map>

using namespace cpp11;
namespace writable = cpp11::writable;

// Copy a named column of a data.frame-like object into a std::vector.
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

// Copy elements of a list item into a std::vector.
template <typename T1, typename T2>
void edges_copy_list (
        const list &li,
        const R_xlen_t i,
        std::vector <T2> &result)
{
    T1 s = li [i];
    result.resize (static_cast <size_t> (s.size ()));
    std::copy (s.begin (), s.end (), result.begin ());
}

inline size_t expand_edges::count_edges (
        const EdgeMapType &edge_map,
        const std::vector <std::string> &edges) {

    size_t len = 0;

    for (auto e: edges) {
        if (edge_map.find (e) == edge_map.end ()) {
            len++;
        } else {
            len += edge_map.at (e).size ();
        }
    }

    return len;
}

void expand_edges::fill_edges (
        const EdgeMapType &edge_map,
        const std::vector <std::string> &edges,
        cpp11::writable::strings &edges_new) {

    size_t i = 0;

    for (auto e: edges) {
        if (edge_map.find (e) == edge_map.end ()) {
            edges_new [i++] = e;
        } else {
            const std::vector <std::string> edges_temp = edge_map.at (e);
            for (auto et: edges_temp) {
                edges_new [i++] = et;
            }
        }
    }
}

[[cpp11::register]]
writable::list cpp_expand_edges(const list paths, const list edge_map_in,
        const bool paths_are_list = false) {


    std::vector <std::string> edge_old, edge_new;

    edges_copy_column <strings, std::string> (edge_map_in, "edge_old", edge_old);
    edges_copy_column <strings, std::string> (edge_map_in, "edge_new", edge_new);

    EdgeMapType edge_map;

    const size_t n = edge_old.size ();
    for (size_t i = 0; i < n; i++) {

        std::vector <std::string> edge_set;
        if (edge_map.find (edge_new [i]) != edge_map.end ()) {
            edge_set = edge_map.at (edge_new [i]);
            edge_map.erase (edge_new [i]);
        }
        edge_set.push_back (edge_old [i]);

        edge_map.emplace (edge_new [i], edge_set);
    }

    cpp11::writable::list out (static_cast <R_xlen_t> (paths.size ()));

    R_xlen_t i = 0;

    for (const list pi: paths) {

        std::vector <std::string> edges_i;
        edges_copy_column <strings, std::string> (pi, "edge_", edges_i);

        size_t len = expand_edges::count_edges (edge_map, edges_i);

        cpp11::writable::strings edges_new (static_cast <R_xlen_t> (len));
        expand_edges::fill_edges (edge_map, edges_i, edges_new);

        out [i++] = edges_new;
    }

    return out;
}
