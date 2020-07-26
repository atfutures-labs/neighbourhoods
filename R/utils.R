
swap_cols <- function (x, nms) {
    temp <- x [[nms [1] ]]
    x [[nms [1] ]] <- x [[nms [2] ]]
    x [[nms [2] ]] <- temp
    return (x)
}

# measures extent to which each row of nbs$.vx0_(x,y) is to the left of
# this_edge
to_left0 <- function (this_edge, nbs) {
    ret <- (this_edge$.vx1_x - this_edge$.vx0_x) * (nbs$.vx0_y - this_edge$.vx0_y) -
        (nbs$.vx0_x - this_edge$.vx0_x) * (this_edge$.vx1_y - this_edge$.vx0_y)
    ret [ret == 0] <- -Inf
    return (ret)
}

# same but for nbs$.vx1_(x,y)
to_left1 <- function (this_edge, nbs) {
    ret <- (this_edge$.vx1_x - this_edge$.vx0_x) * (nbs$.vx1_y - this_edge$.vx0_y) -
        (nbs$.vx1_x - this_edge$.vx0_x) * (this_edge$.vx1_y - this_edge$.vx0_y)
    ret [ret == 0] <- -Inf
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
