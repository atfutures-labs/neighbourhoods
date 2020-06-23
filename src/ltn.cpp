#include <Rcpp.h>

#include <set>
#include <unordered_set>
#include <unordered_map>

namespace LTN{

const double VERY_SMALL = 1.0e-12;

template <typename T>
struct OneVert
{

    OneVert (std::string id, T wt): 
        _id (id), _wt (wt) {}
    
    std::string _id;
    T _wt;

    std::string getid () const { return _id;   }
    T getwt () const { return _wt;   }
};

template <typename T>
struct by_wt
{
    bool operator () (const OneVert <T> &lhs, const OneVert <T> &rhs)
    {
        if (fabs (lhs._wt - rhs._wt) < LTN::VERY_SMALL)
        {
            //return lhs._id < rhs._id;
            return true; // pick first edge
        } else
            return lhs._wt < rhs._wt;
    }
}; 

typedef std::set <OneVert <double>, by_wt <double> > VertSetDbl;

typedef std::unordered_map <std::string, int> RidgeVertMap;

struct pair_hash
{
    template <class T1, class T2>
        std::size_t operator () (std::pair <T1, T2> const &pair) const
        {
            std::size_t h1 = std::hash <T1> () (pair.first);
            std::size_t h2 = std::hash <T2> () (pair.second);

            return h1 ^ h2;
        }
};

typedef std::unordered_map <std::string, double> EdgeMapType;
typedef std::unordered_map <std::string, EdgeMapType> GraphType;

} // end namespace LTN

//' test
//' @noRd
// [[Rcpp::export]]
Rcpp::CharacterVector test (Rcpp::DataFrame net, Rcpp::DataFrame verts)
{
    const size_t nedges = net.nrow (),
                 nverts = verts.nrow ();

    std::vector <std::string> vfr = net [".vx0"],
                              vto = net [".vx1"];
    std::vector <std::string> v = verts ["id"];
    std::vector <double> centrality = verts ["centrality"];

    // --------------------   INITIAL SETUP   -------------------- 
    // 
    // Map of vertex IDs to iterate up along vertex centrality
    std::unordered_map <std::string, double> seed_verts;
    // Map of all vertex IDs held throughout to simply extract centrality values
    std::unordered_map <std::string, double> vert_to_cent_map;

    // Set of vertex IDs and centrality used to iterate through ordered
    // centrality values
    LTN::VertSetDbl vert_set;

    for (size_t i = 0; i < nverts; i++)
    {
        if (fabs (centrality [i]) < LTN::VERY_SMALL)
            continue;

        seed_verts.emplace (v [i], centrality [i]);

        vert_set.insert (LTN::OneVert <double> (v [i], centrality [i]));

        vert_to_cent_map.emplace (v [i], centrality [i]);
    }

    // Map of .vx1 -> .vx0 (to -> from), to map each vertex to all connected
    // ones; in other words a standard vert-based graph
    std::unordered_map <std::string, std::unordered_set <std::string>> vert_map;

    LTN::GraphType graph;

    for (size_t i = 0; i < nedges; i++)
    {
        // verts with centrality == 0 are escluded
        if (seed_verts.find (vfr [i]) == seed_verts.end () ||
                seed_verts.find (vto [i]) == seed_verts.end ())
            continue;

        std::unordered_set <std::string> vert_list;
        if (vert_map.find (vfr [i]) != vert_map.end ())
        {
            vert_list = vert_map.at (vfr [i]);
            vert_map.erase (vfr [i]);
        }
        vert_list.emplace (vto [i]);
        vert_map.emplace (vfr [i], vert_list);

        LTN::EdgeMapType edge_map;
        if (graph.find (vfr [i]) != graph.end ())
        {
            edge_map = graph.at (vfr [i]);
            graph.erase (vfr [i]);
        }
        if (vert_to_cent_map.find (vto [i]) != vert_to_cent_map.end ())
            edge_map.emplace (vto [i], vert_to_cent_map.at (vto [i]));

        graph.emplace (vfr [i], edge_map);
    }

    LTN::RidgeVertMap ridge_verts;

    // Data structures:
    // 1. vert_set An ordered set of OneVert, ordered by centrality
    // 2. seed_verts A map from each vertex to centrality value
    // 3. vert_map A map from each vertex to all vertices that connect from
    //      there.

    // --------------------   LOOP TO EXTRACT LTNS   -------------------- 
    while (seed_verts.size () > 0)
    {
        LTN::OneVert <double> this_vert = *vert_set.begin ();
        vert_set.erase (vert_set.begin ());

        const std::string this_id = this_vert.getid ();
        seed_verts.erase (this_id);

        // verts with centrality == 0 are not in vert_map, so skip:
        if (vert_map.find (this_id) == vert_map.end ())
            continue;

        const double this_wt = this_vert.getwt ();

        LTN::VertSetDbl this_ltn;
        this_ltn.insert (LTN::OneVert <double> (this_id, this_wt));

        std::unordered_set <std::string> ltn_ridge;

        int ltn_size = 1;
        while (this_ltn.size () > 0)
        {
            LTN::OneVert <double> next_vert = *this_ltn.begin ();
            this_ltn.erase (this_ltn.begin ());

            const std::string next_id = next_vert.getid ();
            if (graph.find (next_id) == graph.end ())
                continue;

            LTN::EdgeMapType neighbours = graph.at (next_id);

            bool is_ridge = true;

            for (auto n: neighbours)
            {
                if (n.second > next_vert.getwt ())
                {
                    this_ltn.emplace (n.first, n.second);
                    is_ridge = false;
                    ltn_size++;
                }
            }

            if (is_ridge)
            {
                if (ridge_verts.find (next_id) == ridge_verts.end ())
                    ridge_verts.emplace (next_id, 1);
                else
                {
                    int ridge_count = ridge_verts.at (next_id);
                    ridge_verts.erase (next_id);
                    ridge_verts.emplace (next_id, ++ridge_count);
                }
            }
        }
    }

    int count = 0;
    for (auto r: ridge_verts)
        if (r.second > 1)
            count++;

    Rcpp::CharacterVector res (count);
    count = 0;
    for (auto r: ridge_verts)
        if (r.second > 1)
            res (count++) = r.first;

    return res;
}
