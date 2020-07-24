#' ltn_cycles
#'
#' Get the minimal cycles of an undirected version of a \pkg{dodgr} street
#' network in contracted and undirected form.
#'
#' @param x An \pkg{dodgr} street network processed with the
#' `dodgr_contract_graph` and `merge_directed_graph` functions.
#' @return A list of the minimal cycles of the street network, each of which has
#' three columns of (`.vx0`, `.vx1`, `.edge_`).
#' @export
ltn_cycles <- function (x) {

    start_edge <- 1
    paths <- list ()
    holds <- NULL
    dat <- list (x = x, holds = NULL)

    res <- get_next_cycle (dat, paths, start_edge)
    start_edge <- which (x$edge_ == dat$holds [1])
    dat$holds <- dat$holds [-1]
    dat <- res$dat
    paths <- res$paths

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

get_next_cycle <- function (dat, paths, start_edge = 1) {

    this_edge <- dat$x [start_edge, ]
    if (this_edge$edge_ %in% dat$holds)
        stop ("This should not happen")

    nbs <- get_nbs (dat$x, this_edge)
    if (nrow (nbs) == 0) {
        this_edge <- swap_rows (this_edge, c (".vx0", ".vx1"))
        this_edge <- swap_rows (this_edge, c (".vx0_x", ".vx1_x"))
        this_edge <- swap_rows (this_edge, c (".vx0_y", ".vx1_y"))
        nbs <- get_nbs (dat$x, this_edge)
    }

    dat$path <- this_edge [, c (".vx0", ".vx1", "edge_")]

    tl0 <- to_left0 (this_edge, nbs)
    tl1 <- to_left1 (this_edge, nbs)
    if (max (tl1) > max (tl0)) {
        dat$left_nb <- nbs [which.max (tl1), ]
    } else {
        dat$left_nb <- nbs [which.max (tl0), ]
    }

    other_nbs <- nbs$edge_ [which (!nbs$edge_ %in% c (dat$left_nb$edge_, dat$path$edge_))]
    all_edges <- do.call (rbind, paths)$edge_
    dat$holds <- c (dat$holds, other_nbs [which (!other_nbs %in% c (dat$holds, all_edges))])

    while (!tail (dat$path$.vx1, 1) %in% dat$path$.vx0) {
        dat <- cycle_iterator (dat)
    }

    # remove all traced edges from holds
    dat$holds <- dat$holds [which (!dat$holds %in% c (dat$path$edge_, all_edges))]
    # add path to paths either (1) if it does not exist, or (2) if it is a
    # subset of an existing path. In 2nd case, existing longer paths are
    # deleted.
    i <- match (tail (dat$path$.vx1, 1), dat$path$.vx0)
    p_start <- dat$path$edge_ [1]
    dat$path <- dat$path [i:nrow (dat$path), ]
    #dat$holds <- dat$holds [which (!dat$holds %in% dat$path$edge_)]
    # check whether path is a sub-path of any others:
    add_path <- TRUE
    if (length (paths) > 0) {
        chk <- vapply (paths, function (i) all (dat$path$edge_ %in% i$edge_), logical (1))
        if (any (chk)) {
            nrows <- vapply (paths [which (chk)], nrow, integer (1))
            if (all (nrow (dat$path) < nrows)) {
                paths [which (chk)] <- NULL
            } else {
                add_path <- FALSE
            }
        }
    }
    if (add_path) {
        paths [[length (paths) + 1]] <- dat$path
    }

    dat$path <- dat$left_nb <- NULL

    return (list (paths = paths, dat = dat))
}

# Iterate through a cycle always turning left. May return something larger than
# a minimal cycle.
cycle_iterator <- function (dat) {

    this_edge <- dat$left_nb
    dat$path <- rbind (dat$path, this_edge [, c (".vx0", ".vx1", "edge_")])

    nbs <- get_nbs (dat$x, this_edge)

    tl0 <- to_left0 (this_edge, nbs)
    tl1 <- to_left1 (this_edge, nbs)
    if (max (tl1) > max (tl0)) {
        dat$left_nb <- nbs [which.max (tl1), ]
    } else {
        dat$left_nb <- nbs [which.max (tl0), ]
    }

    other_nbs <- nbs$edge_ [which (nbs$edge_ != dat$left_nb$edge_)]
    dat$holds <- c (dat$holds, other_nbs [which (!other_nbs %in% dat$holds)])

    return (dat)
}
