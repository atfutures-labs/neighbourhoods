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

    x <- preprocess_network (x, duplicate = TRUE)

    dat <- list (x = x)

    pr <- proc.time ()

    paths <- list (paths = list (),
                   path_hashes = NULL)

    dat$edges <- unique (x$edge_)

    paths <- trace_all_edges (dat, paths, start_edge = 1, left = TRUE)

    p <- path_edge_count (paths)
    edges <- p$edge_ [which (p$n == 1)]
    edges <- c (edges, paste0 (edges, "_rev"))
    dat$edges <- edges [which (edges %in% x$edge_)]

    paths <- trace_all_edges (dat, paths, start_edge = 1, left = FALSE)

    paths <- rm_isolated_polygons (x, paths)
    x <- rm_isolated_edges (x, paths)

    dat$edges <- get_restart_edges (paths, x)
    dat$x <- x

    paths <- trace_all_edges (dat, paths, start_edge = 1)
    dat$edges <- get_restart_edges (paths, x)
    paths <- trace_all_edges (dat, paths, start_edge = 1, left = FALSE)

    paths <- reduce_paths (paths$paths)

    pr <- round ((proc.time () - pr) [3], digits = 1)
    message ("Found ", length (paths), " mininal cycles in ",
             pr, " seconds")

    return (paths)
}

#' trace all paths from one set of starting edges
#'
#' starting edges are in 'dat$edges', with the network itself in 'dat$x'.
#' @return List of paths and associated hashes
#' @noRd
trace_all_edges <- function (dat, paths, start_edge, left = TRUE) {

    count <- 0

    while (length (dat$edges) > 0) {

        dat <- trace_next_cycle (dat, start_edge, left)

        if (!is.null (dat$path))
            paths <- add_path (dat, paths)

        count <- count + 1
        message ("\r", length (paths$paths), " paths after ", count,
                 " iterations with ", length (dat$edges),
                 " edges left to traverse   ", appendLF = FALSE)
    }
    message ()

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

trace_next_cycle <- function (dat, start_edge = 1, left = TRUE) {

    trace_one_cycle <- function (dat, start_edge, left) {
        dat <- start_next_cycle (dat, start_edge = start_edge, left)

        while (!utils::tail (dat$path$.vx1, 1) %in% dat$path$.vx0 &&
               nrow (dat$left_nb) > 0) {
            dat <- cycle_iterator (dat, left)
            if (is.null (dat$left_nb))
                break
        }
        return (dat)
    }

    dat <- trace_one_cycle (dat, start_edge, left)

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

start_next_cycle <- function (dat, start_edge = 1, left = TRUE) {

    this_edge <- dat$x [match (dat$edges [start_edge], dat$x$edge_), ]

    nbs <- get_nbs (dat$x, this_edge)

    dat$path <- this_edge [, c (".vx0", ".vx1", "edge_")]

    if (nrow (nbs) == 0) {
        dat$left_nb <- NULL
    } else {
        if (nrow (nbs) == 1) {
            tl <- 1L
        } else {
            tl <- to_left (this_edge, nbs, left)
        }

        dat$left_nb <- nbs [tl, ]
    }

    return (dat)
}

# Iterate through a cycle always turning left. May return something larger than
# a minimal cycle.
cycle_iterator <- function (dat, left = TRUE) {

    this_edge <- dat$left_nb
    dat$path <- rbind (dat$path, this_edge [, c (".vx0", ".vx1", "edge_")])

    nbs <- get_nbs (dat$x, this_edge)

    if (nrow (nbs) == 0) {
        dat$left_nb <- NULL
    } else {
        if (nrow (nbs) == 1) {
            tl <- 1L
        } else {
            tl <- to_left (this_edge, nbs, left)
        }

        dat$left_nb <- nbs [tl, ]
    }

    return (dat)
}

#' tally edges from all paths
#'
#' Bind paths into a single object and add column of edge number counts. This is
#' used to identify start edges for second pass of 'trace_all_edges', for which
#' any edges which occur in 2 distinct paths must have polygons enclosing both
#' sides, so need not be examined as candidates to generate new paths.
#' @noRd
path_edge_count <- function (paths) {

    p <- lapply (seq_along (paths$paths), function (i) {
                 paths$paths [[i]]$pathnum <- i
                 return (paths$paths [[i]])   })
    p <- do.call (rbind, p)

    edge_table <- table (p$edge_)
    p$n <- edge_table [match (p$edge_, names (edge_table))]

    return (p)
}

#' Identify and remove isolated polygons from list of paths.
#'
#' Isolated polygons are groups of one or more polygons which are only connected
#' along a single edge to the main component. These first require all polygons
#' to be grouped (via `dodgr_components`), then numbers of connections counted
#' between these groups. Any groups of polygons with only a single connection
#' are isolated, and are therefore removed from the main `paths` list.
#'
#' This is necessary because isolated polygons only connected by a single path
#' are "attractors" for left-trace algorithms, which converge towards and close
#' around these polygons, preventing tracing of larger polygons which may
#' enclose them.
#' @noRd
rm_isolated_polygons <- function (x, paths) {

    # all component numbers to main network:
    all_edges <- unique (do.call (rbind, paths$paths)$edge_)
    all_edges <- c (all_edges, paste0 (all_edges, "_rev"))
    x_cut <- x [which (x$edge_ %in% all_edges), ]
    x_cut$component <- NULL
    x_cut <- dodgr::dodgr_components (x_cut)
    index <- match (x_cut$edge_, x$edge_)
    x$component <- NA_integer_
    x$component [index] <- x_cut$component

    # find number of connections from each component
    index_no_comp <- which (is.na (x$component))
    verts_no_comp <- unique (c (x$.vx0 [index_no_comp],
                                x$.vx1 [index_no_comp]))
    ncomps <- length (which (!is.na (unique (x$component))))
    nconn <- vapply (seq (ncomps), function (i) {
                         index <- which (x$component == i)
                         comp_verts <- unique (c (x$.vx0 [index],
                                                  x$.vx1 [index]))
                         m <- match (comp_verts, verts_no_comp)
                         return (length (which (!is.na (m))))
                     },
                     integer (1))

    all_edges <- gsub ("\\_rev$", "", x$edge_)
    # get component numbers of paths:
    comp_numbs <- vapply (paths$paths, function (i) {
                              index <- which (all_edges %in% i$edge_)
                              unique (x$component [index])  },
                              integer (1))
    comp_isolated <- which (nconn == 1)
    paths_isolated <- which (comp_numbs %in% comp_isolated)
    paths_connected <- which (!comp_numbs %in% comp_isolated)
    paths$isolated <- paths$paths [paths_isolated]
    paths$paths <- paths$paths [paths_connected]
    # leave path_hashes intact so they still include hashes of isolated polygons

    return (paths)
}

#' Remove isolated edges from network
#'
#' Having identified isolated polygons via `rm_isolated_polygons`, the
#' corresponding edges then need to be removed from the network.
#' @noRd
rm_isolated_edges <- function (x, paths) {

    edges_isolated <- unique (do.call (rbind, paths$isolated)$edge_)
    x_edges <- gsub ("\\_rev$", "", x$edge_)
    index <- which (!x_edges %in% edges_isolated)

    x <- x [index, ]
    x_edges <- x_edges [index]
    index <- which (!duplicated (x_edges))
    x <- preprocess_network (x [index, ], duplicate = TRUE)

    return (x)
}

#' Get edges for second pass of 'trace_all_edges'
#'
#' These are simply edges which only occur in one polygon, because all edges
#' which are in two polygons already form part of all possible enclosed
#' polygons, and can not be used to identify any further polygons. This is less
#' than perfectly efficient, because these edges also include all edges truly
#' external to an entire network. That could maybe be improved sometime?
#'
#' @return List of all edges which occur only once in the list of paths.
#' @noRd
get_restart_edges <- function (paths, x) {

    p <- path_edge_count (paths)
    edges <- p$edge_ [which (p$n == 1)]
    edges <- c (edges, paste0 ("rev_", edges))

    return (edges [which (edges %in% x$edge_)])
}

#' remove any longer paths which entirely enclose shorter paths
#' @noRd
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
