#include "utils.h"

void utils::cut_terminal_rev (std::string &s)
{
    const std::string revend = "_rev";

    const size_t n = s.length ();
    const std::string send = s.substr (n - 4, n - 1);
    if (std::strcmp (send.c_str (), revend.c_str ()) == 0)
        s = s.substr (0, n - 4);
}

// https://stackoverflow.com/questions/1577475/c-sorting-and-keeping-track-of-indexes
template <typename T>
std::vector<size_t> utils::sort_indexes(const std::vector<T> &v) {

  // initialize original index locations
    std::vector<size_t> idx(v.size());
    std::iota(idx.begin(), idx.end(), 0);

  // sort indexes based on comparing values in v
  // using std::stable_sort instead of std::sort
  // to avoid unnecessary index re-orderings
  // when v contains elements of equal values 
    std::stable_sort(idx.begin(), idx.end(),
       [&v](size_t i1, size_t i2) {return v[i1] < v[i2];});

  return idx;
}

template std::vector<size_t> utils::sort_indexes(const std::vector<size_t> &v);
