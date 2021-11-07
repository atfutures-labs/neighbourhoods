#include "clockwise.h"

// Get first edge in clockwise direction from this_edge. The edges don't need to
// be sorted, rather each pair of neighbouring edges are subjected to a binary
// comparison, recording the first clockwise edge of each. Any edges which are
// not the first clockwise edge in any comparison must be excluded, and the
// vector of all such edges will then include all values except the desired one.
// Calculations are based on vector determinants:
// https://stackoverflow.com/questions/6989100/sort-points-in-clockwise-order
std::size_t clockwise::to_left (const double from_x,
        const double from_y,
        const double centre_x,
        const double centre_y,
        const std::vector <double> nbs_x,
        const std::vector <double> nbs_y,
        const bool left) {

    std::vector <std::size_t> nbs_idx = {0, 1};
    std::size_t lefty = to_left_binary (from_x, from_y, centre_x, centre_y,
            nbs_x, nbs_y, nbs_idx, left);

    if (nbs_x.size () > 2) {
        for (std::size_t i = 2; i < nbs_x.size (); i++) {
            std::size_t lefty0 = lefty;
            nbs_idx [0] = lefty;
            nbs_idx [1] = i;
            lefty = to_left_binary (from_x, from_y, centre_x, centre_y,
                    nbs_x, nbs_y, nbs_idx, left);
            lefty = (lefty == 0) ? lefty0 : i;
            lefty0 = lefty;
        }
    }

    return lefty;
}

std::size_t clockwise::to_left_binary (const double from_x,
        const double from_y,
        const double centre_x,
        const double centre_y,
        const std::vector <double> nbs_x,
        const std::vector <double> nbs_y,
        const std::vector <std::size_t> nbs_idx,
        const bool left) {

    std::vector <double> clockwise (2);
    for (std::size_t i = 0; i < 2; i++) {
        clockwise [i] = (nbs_x [nbs_idx [i]] - centre_x) * (from_y - centre_y) -
            (from_x - centre_x) * (nbs_y [nbs_idx [i]] - centre_y);
    }

    std::size_t res = 0L;

    if ((clockwise [0] > 0 && clockwise [1] > 0) ||
            (clockwise [0] < 0 && clockwise [1] < 0)) {
        // if both are either clockwise or anti-clockwise, then compare the 2
        // nbs with one another to find out which is clockwise from the other.
        // clockwise [0] < 0 if nbs [0] clockwise from nbs [1]:
        double clockwise12 = (nbs_x [nbs_idx [0]] - centre_x) *
            (nbs_y [nbs_idx [1]] - centre_y) -
            (nbs_x [nbs_idx [1]] - centre_x) *
            (nbs_y [nbs_idx [0]] - centre_y);
        res = (clockwise12 < 0) ? 0L : 1L;
    } else {
        res = (clockwise [0] > 0) ? 0L : 1L;
    }

    if (!left)
        res = (res == 0) ? 1 : 0;

    return res;
}
