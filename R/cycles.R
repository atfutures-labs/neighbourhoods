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

    x <- preprocess_network (x)

    start_edge <- 1
    dat <- list (x = x)

    pr <- proc.time ()
    count <- 0

    paths <- list (paths = list (),
                   path_hashes = NULL)

    dat$edges <- unique (x$edge_)

    while (length (dat$edges) > 0) {

        dat <- trace_next_cycle (dat, start_edge)

        if (!is.null (dat$path))
            paths <- add_path (dat, paths)

        count <- count + 1
        message ("\r", length (paths$paths), " paths after ", count,
                 " iterations with ", length (dat$edges),
                 " edges left to traverse   ", appendLF = FALSE)
    }
    message ()

    paths <- reduce_paths (paths$paths)

    pr <- round ((proc.time () - pr) [3], digits = 1)
    message ("Found ", length (paths), " mininal cycles in ",
             pr, " seconds")

    return (paths)
}

#' add a path if it does not already exist
#'
#' The edges of a path are the base names, with no "_rev" suffixes to indicate
#' whether or not they point in the original directions. Because paths may start
#' at any point, a unique hash is generated for each path by hashing the set of
#' unique and *sorted* edge identifiers.
#' @noRd
add_path <- function (dat, paths) {
    edges <- gsub ("\\_rev$", "", dat$path$edge_)
    dat$path$edge_ <- edges
    hash <- digest::digest (sort (unique (edges)))
    if (!hash %in% paths$path_hashes) {
        paths$paths [[length (paths$paths) + 1]] <- dat$path
        paths$path_hashes <- c (paths$path_hashes, hash)
    }

    return (paths)
}

get_nbs <- function (x, this_edge) {
    x [which (x$.vx0 == this_edge$.vx1 &
              x$.vx1 != this_edge$.vx0), ]
}

trace_next_cycle <- function (dat, start_edge = 1) {

    trace_one_cycle <- function (dat, start_edge) {
        dat <- start_next_cycle (dat, start_edge = start_edge)

        while (!utils::tail (dat$path$.vx1, 1) %in% dat$path$.vx0 &&
               nrow (dat$left_nb) > 0) {
            dat <- cycle_iterator (dat)
            if (is.null (dat$left_nb))
                break
        }
        return (dat)
    }

    dat <- trace_one_cycle (dat, start_edge)

    dat$edges <- dat$edges [which (!dat$edges %in% dat$path$edge_)]

    # cut path down to enclosing cycle
    i <- match (utils::tail (dat$path$.vx1, 1), dat$path$.vx0)
    if (is.na (i)) {
        dat$path <- NULL
    } else {
        dat$path <- dat$path [i:nrow (dat$path), ]
    }

    return (dat)
}

start_next_cycle <- function (dat, start_edge = 1) {

    this_edge <- dat$x [match (dat$edges [start_edge], dat$x$edge_), ]

    nbs <- get_nbs (dat$x, this_edge)

    dat$path <- this_edge [, c (".vx0", ".vx1", "edge_")]

    if (nrow (nbs) == 0) {
        dat$left_nb <- NULL
    } else {
        if (nrow (nbs) == 1) {
            tl <- 1L
        } else {
            tl <- to_left (this_edge, nbs)
        }

        dat$left_nb <- nbs [tl, ]
    }

    return (dat)
}

# Iterate through a cycle always turning left. May return something larger than
# a minimal cycle.
cycle_iterator <- function (dat) {

    this_edge <- dat$left_nb
    dat$path <- rbind (dat$path, this_edge [, c (".vx0", ".vx1", "edge_")])

    nbs <- get_nbs (dat$x, this_edge)

    if (nrow (nbs) == 0) {
        dat$left_nb <- NULL
    } else {
        if (nrow (nbs) == 1) {
            tl <- 1L
        } else {
            tl <- to_left (this_edge, nbs)
        }

        dat$left_nb <- nbs [tl, ]
    }

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
