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
    dat <- list (x = x)

    pr <- proc.time ()
    count <- 0

    paths <- list (paths = list (),
                   path_hashes = NULL)

    first <- TRUE
    dat$holds <- NA_character_

    while (length (dat$holds) > 0) {

        if (first) {
            dat$holds <- vector (mode = "integer", length = 0)
            first <- FALSE
        }

        dat <- trace_next_cycle (dat, start_edge, reverse = FALSE)
        paths <- add_path (dat, paths)

        dat <- trace_next_cycle (dat, start_edge, reverse = TRUE)
        paths <- add_path (dat, paths)

        start_edge <- which (dat$x$edge_ == dat$holds [1])
        dat$holds <- dat$holds [-1]

        count <- count + 1
        message ("\r", count, " iterations   ", appendLF = FALSE)
    }

    message ()
    pr <- round ((proc.time () - pr) [3], digits = 1)
    message ("\rFinished after ", count, " iterations in ",
             pr, " seconds")

    paths <- reduce_paths (paths$paths)

    message ("Found ", length (paths), " mininal cycles")

    return (paths)
}

add_path <- function (dat, paths) {
    hashes <- c (digest::digest (dat$path$edge_),
                 digest::digest (rev (dat$path$edge_)))
    if (!any (hashes %in% paths$path_hashes)) {
        paths$paths [[length (paths$paths) + 1]] <- dat$path
        paths$path_hashes <- c (paths$path_hashes, hashes)
    }

    return (paths)
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

trace_next_cycle <- function (dat, start_edge = 1, reverse = FALSE) {

    this_edge <- dat$x [start_edge, ]
    if (reverse) {
        this_edge <- swap_rows (this_edge, c (".vx0", ".vx1"))
        this_edge <- swap_rows (this_edge, c (".vx0_x", ".vx1_x"))
        this_edge <- swap_rows (this_edge, c (".vx0_y", ".vx1_y"))
    }

    nbs <- get_nbs (dat$x, this_edge)

    dat$path <- this_edge [, c (".vx0", ".vx1", "edge_")]

    tl0 <- to_left0 (this_edge, nbs)
    tl1 <- to_left1 (this_edge, nbs)
    if (max (tl1) > max (tl0)) {
        dat$left_nb <- nbs [which.max (tl1), ]
    } else {
        dat$left_nb <- nbs [which.max (tl0), ]
    }

    other_nbs <- nbs$edge_ [which (!nbs$edge_ %in% c (dat$left_nb$edge_, dat$path$edge_))]
    dat$holds <- c (dat$holds, other_nbs [which (!other_nbs %in% dat$holds)])

    while (!tail (dat$path$.vx1, 1) %in% dat$path$.vx0) {
        dat <- cycle_iterator (dat)
    }

    # add all traced edges to done, and remove from holds
    dat$done <- unique (c (dat$done, dat$path$edge_))
    dat$holds <- dat$holds [which (!dat$holds %in% dat$done)]

    # cut path down to enclosing cycle
    i <- match (tail (dat$path$.vx1, 1), dat$path$.vx0)
    p_start <- dat$path$edge_ [1]
    dat$path <- dat$path [i:nrow (dat$path), ]

    return (dat)
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

# remove any longer paths which entirely enclose shorter paths
reduce_paths <- function (paths) {
    edges <- lapply (paths, function (i) i$edge_)
    index <- order (vapply (edges, length, integer (1)))
    paths <- paths [index]
    edges <- edges [index]

    n <- length (edges)
    removes <- rep (FALSE, n)

    for (i in seq (n) [-n]) {
        for (j in (i + 1):n) {
            if (all (edges [[i]] %in% edges [[j]]))
                removes [j] <- TRUE
        }
    }

    return (paths [which (!removes)])
}
