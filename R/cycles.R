
swap_rows <- function (x, nms) {
    temp <- x [[nms [1] ]]
    x [[nms [1] ]] <- x [[nms [2] ]]
    x [[nms [2] ]] <- temp
    return (x)
}

# Get the undirected neighbour list, which means reversing the order where
# necessary to ensure sequence is directed from v1 to v0 of the next
# neighbours.
get_nbs <- function (x, this_edge) {
    res <- x [which (x$.vx0 == this_edge$.vx1), ]
    index <- which (x$.vx1 == this_edge$.vx1)
    if (length (index) > 0) {
        res2 <- x [index, ]
        res2 <- swap_rows (res2, c (".vx0", ".vx1"))
        res2 <- swap_rows (res2, c (".vx0_x", ".vx1_x"))
        res2 <- swap_rows (res2, c (".vx0_y", ".vx1_y"))
        res <- rbind (res, res2)
    }
    return (res)
}

to_left0 <- function (this_edge, nbs) {
    ret <- (this_edge$.vx1_x - this_edge$.vx0_x) * (nbs$.vx0_y - this_edge$.vx0_y) -
        (nbs$.vx0_x - this_edge$.vx0_x) * (this_edge$.vx1_y - this_edge$.vx0_y)
    ret [ret == 0] <- -Inf
    return (ret)
}

to_left1 <- function (this_edge, nbs) {
    ret <- (this_edge$.vx1_x - this_edge$.vx0_x) * (nbs$.vx1_y - this_edge$.vx0_y) -
        (nbs$.vx1_x - this_edge$.vx0_x) * (this_edge$.vx1_y - this_edge$.vx0_y)
    ret [ret == 0] <- -Inf
    return (ret)
}

ltn_cycles <- function (x) {
    paths <- list ()
    excluded <- NULL

    path <- get_next_cycle (x, excluded, 1)

    paths [[length (paths) + 1]] <- path
    excluded <- unique (c (excluded, path$edge_))
}

get_next_cycle <- function (x, excluded, start_edge = 1) {

    this_edge <- x [start_edge, ]
    path <- this_edge [, c (".vx0", ".vx1", "edge_")]
    x <- x [-start_edge, ]

    nbs <- get_nbs (x, this_edge)
    if (nrow (nbs) == 0) {
        this_edge <- swap_rows (this_edge, c (".vx0", ".vx1"))
        this_edge <- swap_rows (this_edge, c (".vx0_x", ".vx1_x"))
        this_edge <- swap_rows (this_edge, c (".vx0_y", ".vx1_y"))
        nbs <- get_nbs (x, this_edge)
    }

    tl0 <- to_left0 (this_edge, nbs)
    tl1 <- to_left1 (this_edge, nbs)
    if (max (tl1) > max (tl0)) {
        left_nb <- nbs [which.max (tl1), ]
    } else {
        left_nb <- nbs [which.max (tl0), ]
    }

    other_nbs <- list ()
    if (nrow (nbs) > 1) {
        other_nbs <- list (nbs$edge_ [which (nbs$edge_ != left_nb$edge_)])
        names (other_nbs) <- left_nb$edge_
    }
    these_excluded <- NULL

    dat <- list (x = x,
                 path = path,
                 left_nb = left_nb,
                 other_nbs = other_nbs,
                 excluded = excluded,
                 these_excluded = these_excluded)

    while (nrow (dat$x) > 0) {
        dat <- cycle_iterator (dat)

        if (utils::tail (dat$path$.vx1, 1) %in% dat$path$.vx0)
            break
    }

    i <- match (utils::tail (dat$path$.vx1, 1), dat$path$.vx0)
    path <- dat$path [i:nrow (dat$path), ]
    return (path)
}

cycle_iterator <- function (dat) {

    this_edge <- dat$left_nb
    dat$x <- dat$x [which (dat$x$edge_ != this_edge$edge_), ]
    dat$path <- rbind (dat$path, this_edge [, c (".vx0", ".vx1", "edge_")])

    nbs <- get_nbs (dat$x, this_edge)
    # set default value for left_nb only when nbs has no rows
    nbs <- dat$left_nb <- nbs [which (!nbs$edge_ %in%
                                  c (dat$excluded, dat$these_excluded)), ]

    if (nrow (nbs) > 0) {
        tl0 <- to_left0 (this_edge, nbs)
        tl1 <- to_left1 (this_edge, nbs)
        if (max (tl1) > max (tl0)) {
            dat$left_nb <- nbs [which.max (tl1), ]
        } else {
            dat$left_nb <- nbs [which.max (tl0), ]
        }
    }

    if (nrow (dat$left_nb) == 0 & !utils::tail (dat$path$.vx1, 1) %in% dat$path$.vx0) {
        # step back to terminal point of other_nbs
        dat$left_nb <- unname (unlist (utils::tail (dat$other_nbs, 1)))
        # remove that left_nb from other_nbs:
        if (length (dat$left_nb) == 1) { # remove whole list item:
            dat$other_nbs <- dat$other_nbs [-length (dat$other_nbs)]
        } else { # remove from multiple-entry list item:
            dat$other_nbs [length (dat$other_nbs)] <- dat$left_nb [-1]
            dat$left_nb <- dat$left_nb [1]
        }
        dat$left_nb <- dat$x [match (dat$left_nb, dat$x$edge_), ]

        i <- which (dat$path$edge_ == utils::tail (names (dat$other_nbs), 1))
        dat$these_excluded <- unique (c (dat$these_excluded,
                                         dat$path$edge_ [i:nrow (dat$path)]))
        dat$path <- dat$path [seq (i - 1), ]
        if (dat$left_nb$.vx1 == utils::tail (dat$path$.vx1, 1)) {
            dat$left_nb <- swap_rows (dat$left_nb, c (".vx0", ".vx1"))
            dat$left_nb <- swap_rows (dat$left_nb, c (".vx0_x", ".vx1_x"))
            dat$left_nb <- swap_rows (dat$left_nb, c (".vx0_y", ".vx1_y"))
        }
    } else if (nrow (nbs) > 1) {
        dat$other_nbs [[length (dat$other_nbs) + 1]] <-
            nbs$edge_ [which (nbs$edge_ != dat$left_nb$edge_)]
        names (dat$other_nbs) [length (dat$other_nbs)] <- dat$left_nb$edge_
    }

    return (dat)
}
