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

typedef std::set <OneVert <double>, by_wt <double> > VertSet;

} // end namespace LTN

//' test
//' @noRd
// [[Rcpp::export]]
int test (Rcpp::DataFrame net, Rcpp::DataFrame verts)
{
    const size_t nedges = net.nrow (),
                 nverts = verts.nrow ();

    std::vector <std::string> vfr = net [".vx0"],
                              vto = net [".vx1"];
    std::vector <std::string> v = verts ["id"];
    std::vector <double> centrality = verts ["centrality"];

    // --------------------   INITIAL SETUP   -------------------- 
    // 
    // Map of vertex IDs to vertex centrality
    std::unordered_map <std::string, double> vert_to_cent_map;

    // Set of vertex IDs and centrality used to iterate through ordered
    // centrality values
    LTN::VertSet vert_set;

    for (size_t i = 0; i < nverts; i++)
    {
        if (fabs (centrality [i]) < LTN::VERY_SMALL)
            continue;

        vert_to_cent_map.emplace (v [i], centrality [i]);

        vert_set.insert (LTN::OneVert <double> (v [i], centrality [i]));
    }

    // Map of .vx1 -> .vx0 (to -> from), to map each vertex to all connected
    // ones; in other words a standard vert-based graph
    std::unordered_map <std::string, std::unordered_set <std::string>> vert_map;

    for (size_t i = 0; i < nedges; i++)
    {
        if (vert_to_cent_map.find (vfr [i]) == vert_to_cent_map.end () ||
                vert_to_cent_map.find (vto [i]) == vert_to_cent_map.end ())
            continue;

        std::unordered_set <std::string> vert_list;
        if (vert_map.find (vfr [i]) != vert_map.end ())
        {
            vert_list = vert_map.at (vfr [i]);
            vert_map.erase (vfr [i]);
        }
        vert_list.emplace (vto [i]);
        vert_map.emplace (vfr [i], vert_list);
    }

    // Data structures:
    // 1. vert_set An ordered set of OneVert, ordered by centrality
    // 2. vert_to_cent_map A map from each vertex to centrality value
    // 3. vert_map A map from each vertex to all vertices that connect from
    //      there.

    // --------------------   LOOP TO EXTRACT LTNS   -------------------- 
    int junk = 0;
    //while (vert_to_cent_map.size () > 0)
    while (vert_set.size () > 0)
    {
        junk++;
        LTN::OneVert <double> this_vert = *vert_set.begin ();
        vert_set.erase (vert_set.begin ());

        const std::string this_id = this_vert.getid ();
        vert_to_cent_map.erase (this_id);

        // verts with centrality == 0 are not in vert_map, so skip:
        if (vert_map.find (this_id) == vert_map.end ())
            continue;

        const double this_wt = this_vert.getwt ();

        std::unordered_set <std::string> in_set;
        in_set.emplace (this_id);

        // ordered set of neighbour vertices with wt > this_wt
        LTN::VertSet this_vert_set;
        const std::unordered_set <std::string> these_nbs = vert_map.at (this_id);
        for (auto n: these_nbs)
        {
            if (vert_to_cent_map.find (n) == vert_to_cent_map.end ())
                continue; // vertex already visited
            double wt_n = vert_to_cent_map.at (n);
            if (wt_n > this_wt)
            {
                LTN::OneVert <double> vi = LTN::OneVert <double> (n, wt_n);
                this_vert_set.insert (vi);
                vert_to_cent_map.erase (n);
                vert_set.erase (vi);
                in_set.emplace (n);
            }
        }

        while (this_vert_set.size () > 0)
        {
            LTN::OneVert <double> next_vert = *this_vert_set.begin ();
            const std::string next_id = next_vert.getid ();
            if (vert_map.find (next_id) == vert_map.end ())
                continue; // vertex already visited
            this_vert_set.erase (this_vert_set.begin ());

            const std::unordered_set <std::string> next_nbs = vert_map.at (next_id);
            for (auto n: next_nbs)
            {
                if (vert_to_cent_map.find (n) == vert_to_cent_map.end ())
                    continue;
                double wt_n = vert_to_cent_map.at (n);
                if (wt_n > next_vert.getwt ())
                {
                    LTN::OneVert <double> vi = LTN::OneVert <double> (n, wt_n);
                    this_vert_set.insert (vi);
                    vert_to_cent_map.erase (n);
                    vert_set.erase (vi);
                    in_set.emplace (n);
                }
            }
        }
    }

    return junk;
}
