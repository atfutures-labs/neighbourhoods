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

    edge_list <- lapply (c (TRUE, FALSE), function (i)
                         cycles_cpp (x,
                                     unique (dat$edges),
                                     start_edge_index = 0,
                                     left = i))

    cycle_hash <- function (res, x) {
        vapply (res, function (i) {
                    e <- gsub ("\\_rev$", "", x$edge_ [i])
                    digest::digest (sort (e))
                   },
                   character (1))
    }

    h_l <- cycle_hash (edge_list [[1]], x)
    h_r <- cycle_hash (edge_list [[2]], x)
    index <- which (!h_r %in% h_l)
    edge_list <- c (edge_list [[1]], edge_list [[2]] [index])

    paths <- list (paths = lapply (edge_list, function (i) {
                       j <- x [i, ]
                       j$edge_ <- gsub ("\\_rev$", "", j$edge_)
                       return (j)   }))

    paths <- rm_isolated_polygons (x, paths)
    x0 <- x
    x <- rm_isolated_edges (x, paths)
    edge_map <- match (x$edge_, x0$edge_)

    dat$edges <- get_restart_edges (paths, x)
    dat$x <- x

    edge_list_l <- cycles_cpp (x,
                               unique (dat$edges),
                               start_edge_index = 1,
                               left = TRUE)
    edge_list_l <- lapply (edge_list_l, function (i) edge_map [i])

    h0 <- cycle_hash (edge_list, x0)
    h1 <- cycle_hash (edge_list_l, x)
    index <- which (!h1 %in% h0)
    edge_list <- c (edge_list, edge_list_l [index])

    index <- cpp_reduce_paths (edge_list)
    edge_list <- edge_list [which (!index)]

    paths <- lapply (edge_list, function (i) x0 [i, ])

    pr <- round ((proc.time () - pr) [3], digits = 1)
    message ("Found ", length (paths), " mininal cycles in ",
             pr, " seconds")

    return (paths)
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
    # exclude main component in case that also only has one connection
    comp_isolated <- comp_isolated [comp_isolated > 1]
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
