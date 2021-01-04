
# remove all terminal nodes, and duplicate all edges in reverse direction
preprocess_network <- function (x, duplicate = TRUE) {

    tnodes <- names (which (table (c (x$.vx0, x$.vx1)) == 1))
    while (length (tnodes) > 0) {
        index_out <- which (x$.vx0 %in% tnodes | x$.vx1 %in% tnodes)
        index_in <- seq (nrow (x)) [which (!seq (nrow (x)) %in% index_out)]
        x <- x [index_in, ]
        tnodes <- names (which (table (c (x$.vx0, x$.vx1)) == 1))
    }

    if (duplicate)
        x <- duplicate_network (x)

    return (x)
}

swap_cols <- function (x, nms) {
    temp <- x [[nms [1] ]]              # nolint
    x [[nms [1] ]] <- x [[nms [2] ]]    # nolint
    x [[nms [2] ]] <- temp              # nolint
    return (x)
}

duplicate_network <- function (x) {
    x2 <- x
    x2 <- swap_cols (x2, c (".vx0", ".vx1"))
    x2 <- swap_cols (x2, c (".vx0_x", ".vx1_x"))
    x2 <- swap_cols (x2, c (".vx0_y", ".vx1_y"))
    x2$edge_ <- paste0 (x2$edge_, "_rev")
    rbind (x, x2)
}
