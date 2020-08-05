
swap_cols <- function (x, nms) {
    temp <- x [[nms [1] ]]
    x [[nms [1] ]] <- x [[nms [2] ]]
    x [[nms [2] ]] <- temp
    return (x)
}

# measures extent to which each row of nbs$.vx0_(x,y) is to the left of
# this_edge - this is only for scanning network in reverse direction, and is not
# used.
# nocov start
to_left0 <- function (this_edge, nbs) {
    ret <- (this_edge$.vx1_x - this_edge$.vx0_x) * (nbs$.vx0_y - this_edge$.vx0_y) -
        (nbs$.vx0_x - this_edge$.vx0_x) * (this_edge$.vx1_y - this_edge$.vx0_y)
    ret [ret == 0] <- -Inf
    return (ret)
}
# nocov end

# Angle between 2 lines, obtained by first transforming end point of 2nd line
# into coordinates of first line, then using diamond angle formula
# https://stackoverflow.com/questions/1427422/cheap-algorithm-to-find-measure-of-angle-between-vectors
#
# (x2, y2) can be vectors; all others are single values
diamond_angle <- function (x0, y0, x1, y1, x2, y2) {

    # perpendicular dist from (x2, y2) to line (x0, y0) -> (x1, y1):
    ab_x <- x1 - x0
    ab_y <- y1 - y0
    ae_x <- x2 - x0
    ae_y <- y2 - y0
    be_x <- x2 - x1
    be_y <- y2 - y1

    mod <- sqrt (ab_x ^ 2 + ab_y ^ 2)
    y <- abs (ab_x * ae_y - ab_y * ae_x) / mod
    # then get x from triangle for which d_be ^ 2 = x ^ 2 + y ^ 2
    x <- sqrt (be_x ^ 2 + be_y ^ 2 - y ^ 2)

    # submit those 2 diplacements to diamond formula:
    ret <- rep (NA, length (x))
    index <- which (y >= 0 & x >= 0)
    if (length (index) > 0)
        ret [index] <- y / (x + y)
    index <- which (y >= 0 & x < 0)
    if (length (index) > 0)
        ret [index] <- 1 + x / (-x + y)
    index <- which (y < 0 & x >= 0)
    if (length (index) > 0)
        ret [index] <- 2 - y / (-x - y)
    index <- which (y < 0 & x < 0)
    if (length (index) > 0)
        ret [index] <- 3 + x / (x - y)

    return (ret)
}

# Measure how much each neighbour lies to the left of this_edge, based on area
# formula https://en.wikipedia.org/wiki/Curve_orientation of
# 2A = x1 * y2 - x2 * y1
# see also http://geomalgorithms.com/a03-_inclusion.html
to_left <- function (this_edge, nbs) {
    ret <- (this_edge$.vx1_x - this_edge$.vx0_x) * (nbs$.vx1_y - this_edge$.vx0_y) -
        (nbs$.vx1_x - this_edge$.vx0_x) * (this_edge$.vx1_y - this_edge$.vx0_y)

    #ret [ret == 0] <- -Inf
    return (ret)
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
