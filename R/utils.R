
swap_cols <- function (x, nms) {
    temp <- x [[nms [1] ]]              # nolint
    x [[nms [1] ]] <- x [[nms [2] ]]    # nolint
    x [[nms [2] ]] <- temp              # nolint
    return (x)
}

# Get first edge in clockwise direction from this_edge. The edges don't need to
# be sorted, rather each pair of neighbouring edges are subjected to a binary
# comparison, recording the first clockwise edge of each. Any edges which are
# not the first clockwise edge in any comparison must be excluded, and the
# vector of all such edges will then include all values except the desired one.
# Calculations are based on vector determinants:
# https://stackoverflow.com/questions/6989100/sort-points-in-clockwise-order
to_left <- function (this_edge, nbs) {

    combs <- t (combn (nrow (nbs), 2))
    not_lefties <- apply (combs, 1, function (i) {
                      lefty <- to_left_binary (this_edge, nbs [i, ])
                      not_lefty <- (2:1) [lefty]
                      i [not_lefty] })
    return (which (!seq (nrow (nbs)) %in% not_lefties))
}

to_left_binary <- function (this_edge, nbs) {

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

    return (out)
}

# remove all terminal nodes, and duplicate all edges in reverse direction
preprocess_network <- function (x) {

    tnodes <- names (which (table (c (x$.vx0, x$.vx1)) == 1))
    while (length (tnodes) > 0) {
        index_out <- which (x$.vx0 %in% tnodes | x$.vx1 %in% tnodes)
        index_in <- seq (nrow (x)) [which (!seq (nrow (x)) %in% index_out)]
        x <- x [index_in, ]
        tnodes <- names (which (table (c (x$.vx0, x$.vx1)) == 1))
    }

    duplicate_network (x)
}

duplicate_network <- function (x) {
    x2 <- x
    x2 <- swap_cols (x2, c (".vx0", ".vx1"))
    x2 <- swap_cols (x2, c (".vx0_x", ".vx1_x"))
    x2 <- swap_cols (x2, c (".vx0_y", ".vx1_y"))
    x2$edge_ <- paste0 (x2$edge_, "_rev")
    rbind (x, x2)
}
