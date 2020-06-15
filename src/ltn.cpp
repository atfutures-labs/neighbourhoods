#include <Rcpp.h>

#include <set>
#include <unordered_set>
#include <unordered_map>

namespace LTN{

struct OneEdge
{

    OneEdge (std::string id, std::string fr, std::string to, double wt): 
        _id (id), _fr (fr), _to (to), _wt (wt) {}
    
    std::string _id;
    std::string _fr;
    std::string _to;
    double _wt;

    std::string getid () const { return _id;   }
    std::string getfr () const { return _fr;   }
    std::string getto () const { return _to;   }
    double getwt () const { return _wt;   }
};

struct by_wt
{
    bool operator () (const OneEdge& lhs, const OneEdge& rhs)
    {
        if (fabs (lhs._wt - rhs._wt) < 1.0e-12)
        {
            //return lhs._id < rhs._id;
            return true; // pick first edge
        } else
            return lhs._wt < rhs._wt;
    }
}; 

typedef std::set <OneEdge, by_wt> EdgeSet;

} // end namespace LTN

//' test
//' @noRd
// [[Rcpp::export]]
int test (Rcpp::DataFrame graph)
{
    const size_t n = graph.nrow ();

    std::vector <std::string> vfr = graph [".vx0"],
                              vto = graph [".vx1"],
                              edge = graph ["edge_"];
    std::vector <double> centrality = graph ["centrality"];

    // --------------------   INITIAL SETUP   -------------------- 
    // 
    // Set of edge IDs and centrality used to iterate through ordered centrality
    // values
    LTN::EdgeSet edge_set;

    // Map of edge IDs and centrality - needed for efficient lookup of
    // centrality values for edges in the above set
    std::unordered_map <std::string, double> edge_to_cent_map;

    // Map of .vx1 -> .vx0 (to -> from), to map edge ends to starts of subsequent edges
    std::unordered_map <std::string, std::unordered_set <std::string>> vert_map;

    // Map of .vx0 -> edge, to find all edges for each start vertex
    std::unordered_map <std::string, std::unordered_set <std::string>> v0_to_edge_map;

    for (size_t i = 0; i < n; i++)
    {
        if (centrality [i] > 0.0)
        {
            edge_set.insert (LTN::OneEdge (edge [i], vfr [i], vto [i], centrality [i]));

            edge_to_cent_map.emplace (edge [i], centrality [i]);

            std::unordered_set <std::string> vert_list;
            if (vert_map.find (vto [i]) != vert_map.end ())
            {
                vert_list = vert_map.at (vto [i]);
                vert_map.erase (vto [i]);
            }
            vert_list.emplace (vfr [i]);
            vert_map.emplace (vto [i], vert_list);

            std::unordered_set <std::string> edge_list;
            if (v0_to_edge_map.find (vfr [i]) != v0_to_edge_map.end ())
            {
                edge_list = v0_to_edge_map.at (vfr [i]);
                v0_to_edge_map.erase (vfr [i]);
            }
            edge_list.insert (edge [i]);
            v0_to_edge_map.emplace (vfr [i], edge_list);
        }
    }

    // Data structures:
    // 1. edge_set An ordered set of OneEdge, ordered by centrality
    // 2. edge_to_cent_map A map from each edge to centrality value
    // 3. vert_map A map from to (.vx1) vertices to all from (.vx0) vertices
    //      that connect to there.
    // 4. v0_to_edge_map Map from .vx0 vertices to all edges that extend from
    //      there.

    // --------------------   LOOP TO EXTRACT LTNS   -------------------- 
    while (edge_to_cent_map.size () > 0)
    {
        LTN::OneEdge this_edge = *edge_set.begin ();
        edge_set.erase (edge_set.begin ());
        edge_to_cent_map.erase (this_edge.getid ());

        // find all connecting edges
        const std::string edge_v1 = this_edge.getto ();
        if (v0_to_edge_map.find (edge_v1) != v0_to_edge_map.end ())
        {
            std::unordered_set <std::string> edge_list = v0_to_edge_map.at (edge_v1);
            for (auto it: edge_list)
            {
                if (edge_to_cent_map.at (it) > this_edge.getwt ())
                {

                }
            }
        }
    }

    return 1;
}
