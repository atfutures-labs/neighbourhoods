#include "cycles.h"

void cycles::fill_network (Network &network,
        const std::vector <std::string> edges,
        const std::vector <std::string> v0,
        const std::vector <std::string> v1)
{

    const size_t n = edges.size ();

    network.edges.resize (n);

    for (size_t i = 0; i < n; i++)
    {

        network.edges [i].edge = edges [i];
        network.edges [i].v0 = v0 [i];
        network.edges [i].v1 = v1 [i];
        network.edge_map.emplace (edges [i], i);

        std::vector <size_t> v0_vec;
        if (network.v0_map.find (v0 [i]) !=
                network.v0_map.end ())
        {
            v0_vec = network.v0_map.at (v0 [i]);
            network.v0_map.erase (v0 [i]);
        }
        v0_vec.push_back (i);
        network.v0_map.emplace (v0 [i], v0_vec);

        std::vector <size_t> v1_vec;
        if (network.v0_map.find (v1 [i]) !=
                network.v0_map.end ())
        {
            v1_vec = network.v0_map.at (v1 [i]);
            network.v0_map.erase (v1 [i]);
        }
        v1_vec.push_back (i);
        network.v1_map.emplace (v1 [i], v1_vec);
    } // end for i
}

std::vector <size_t> get_nbs (const Network &network,
        const size_t this_edge)
{
    const std::string v0 = network.edges [this_edge].v0;
    const std::string v1 = network.edges [this_edge].v1;

    const std::vector <size_t> v1_nbs = network.v1_map.at (v1); 
    bool v1_has_v0 = false;
    for (auto i: v1_nbs)
    {
        if (network.edges [i].v0 == v0)
        {
            v1_has_v0 = true;
            break;
        }
    }
    size_t nnbs = v1_nbs.size ();
    if (v1_has_v0)
        nnbs--;

    std::vector <size_t> nbs (nnbs);
    size_t i = 0;
    for (size_t j = 0; j < v1_nbs.size (); j++)
    {
        if (network.edges [j].v0 != v0)
            nbs [i++] = v1_nbs [j];
    }

    return nbs;
}

void start_next_cycle (const Network &network,
        PathData &pathData,
        const std::string &start_edge)
{
    if (network.edge_map.find (start_edge) ==
            network.edge_map.end ())
        Rcpp::stop ("edge not found");

    //size_t this_edge = network.edge_map.at (start_edge);
}
