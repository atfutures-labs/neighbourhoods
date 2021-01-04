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

    paths <- trace_all_edges (dat, paths, start_edge = 1)

    x <- rm_isolated_edges (x, paths)

    paths <- rm_isolated_polygons (x, paths)

    dat$edges <- get_restart_edges (paths, x)
    dat$x <- x

    paths <- trace_all_edges (dat, paths, start_edge = 1)

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
trace_all_edges <- function (dat, paths, start_edge) {

    count <- 0

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

#' remove isolated polygon edges from the network
#'
#' While this could be done with the edges of the isolated polygons, themselves
#' identified via `find_isolated_polygons`, this can also disconnect resultant
#' networks. The primary algorithm presumes a network to be a single connected
#' component, and so the approach used here is to simply use `dodgr` functions
#' to identify and extract that component.
#' @noRd
rm_isolated_edges <- function (x, paths) {

    all_edges <- unique (do.call (rbind, paths$paths)$edge_)
    x <- x [which (x$edge_ %in% all_edges), ]
    x$component <- NULL
    x <- dodgr::dodgr_components (x)
    x <- x [which (x$component == 1), ]
    x <- dodgr::merge_directed_graph (x)

    x <- preprocess_network (x, duplicate = TRUE)

    return (x)
}

#' Identify and remove isolated polygons from list of paths.
#'
#' Isolated polygons are groups of one or more polygons which do not share any
#' edges with any other polygons. These can not be idenfied through edge counts
#' alone, because they may arise in groups. The only appropriate way is
#' therefore to idenify edges in the primary network component using the
#' preceding `rm_isolated_edges` function, and then to identify isolated
#' polygons as any which have no edges in common with the major network
#' component.
#' @noRd
rm_isolated_polygons <- function (x, paths) {

    p <- path_edge_count (paths)
    all_edges <- unique (gsub ("\\_rev$", "", x$edge_))
    p$in_net <- vapply (p$edge_, function (i) i %in% all_edges,
                        logical (1))

    # split back to original list
    p <- split (p, f = factor (p$pathnum))
    is_isolated <- vapply (p, function (i)
                           !any (i$in_net),
                           logical (1))

    paths$isolated <- paths$paths [which (is_isolated)]
    paths$paths <- paths$paths [which (!is_isolated)]
    paths$path_hashes <- paths$path_hashes [which (!is_isolated)]

    return (paths)
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
