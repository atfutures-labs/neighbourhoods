# Get first edge in clockwise direction from this_edge. The edges don't need to
# be sorted, rather each pair of neighbouring edges are subjected to a binary
# comparison, recording the first clockwise edge of each. Any edges which are
# not the first clockwise edge in any comparison must be excluded, and the
# vector of all such edges will then include all values except the desired one.
# Calculations are based on vector determinants:
# https://stackoverflow.com/questions/6989100/sort-points-in-clockwise-order
to_left <- function (this_edge, nbs, left = TRUE) {

    lefty <- to_left_binary (this_edge, nbs [1:2, ], left = left)
    nb_left <- nbs [lefty, ]

    for (i in seq (nrow (nbs)) [-(1:2)]) {
        nbs_i <- rbind (nbs [lefty, ], nbs [i, ])
        lefty <- to_left_binary (this_edge, nbs_i, left = left)
        nb_left <- nbs_i [lefty, , drop = FALSE]
    }

    return (match (nb_left$edge_, nbs$edge_))
}

to_left_binary <- function (this_edge, nbs, left = TRUE) {

    centre_x <- this_edge$.vx1_x
    centre_y <- this_edge$.vx1_y

    res <- (nbs$.vx1_x - centre_x) * (this_edge$.vx0_y - centre_y) -
        (this_edge$.vx0_x - centre_x) * (nbs$.vx1_y - centre_y)

    if (sign (res [1]) == sign (res [2])) {
        # if both are either clockwise or anti-clockwise, then compare the 2 nbs
        # with one another to find out which is clockwise from the other.
        # res1 < 0 if nbs [1] clockwise from nbs [2]:
        res1 <- (nbs$.vx1_x [1] - centre_x) * (nbs$.vx1_y [2] - centre_y) -
            (nbs$.vx1_x [2] - centre_x) * (nbs$.vx1_y [1] - centre_y)
        out <- ifelse (res1 < 0, 1L, 2L)
    } else {
        out <- which (res > 0)
    }

    if (!left)
        out <- (2:1) [out]

    return (out)
}
