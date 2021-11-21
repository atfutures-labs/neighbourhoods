
#' Construct adjacency matrix of neighbourhood cycles
#'
#' @param cycles List of cycles obtained from \link{network_cycles}.
#' @param measure Measure of aggregate centrality; acceptable values are
#' "median", "mean", "max", "min".
#' @return A `data.frame` of three columns:
#' \enumerate{
#' \item from - cycle from which connection is made
#' \item to - cycle to which connection is made
#' \item centrality - Median centrality of edges connecting those two cycles
#' }
#' @export
adjacent_cycles <- function (cycles, measure = "median") {

    measure <- match.arg (measure, c ("median", "mean", "max", "min"))

    paths <- lapply (seq_along (paths), function (i)
                     cbind (paths [[i]], cycle = i))
    paths_df <- do.call (rbind, paths) %>%
        dplyr::select (edge_, centrality, cycle) %>%
        dplyr::mutate (edge_ = gsub ("\\_rev$", "", edge_))

    # dplyr here is around 10 times slower
    nbs <- lapply (unique (paths_df$cycle), function (i) {
                       index_in <- which (paths_df$cycle == i)
                       index_out <- which (!seq (nrow (paths_df)) %in% index_in)
                       e_i <- paths_df$edge_ [index_in]
                       e_j <- paths_df [index_out, ]
                       e_j <- e_j [which (e_j$edge_ %in% e_i), ]
                       if (nrow (e_j) == 0L)
                           return (NULL)
                       res <- lapply (split (e_j, f = as.factor (e_j$cycle)),
                                      function (i)
                                      list (cycle = i$cycle [1],
                                            centrality =
                                                do.call (measure,
                                                         list (i$centrality)),
                                            edges = i$edge_))
                       edges <- lapply (res, function (i) i$edges)
                       res <- t (vapply (res, function (i)
                                         c (i$cycle, i$centrality),
                                         numeric (2)))
                       res <- data.frame (from = i,
                                          to = res [, 1],
                                          centrality = res [, 2],
                                          edges = I (edges))

                       return (res)
                     })

    nbs <- do.call (rbind, nbs)
    row.names (nbs) <- NULL

    return (nbs)
}

#' Unconstract lists of shared neighbour edges returned from
#' \link{adjacent_cycles}.
#'
#' @noRd
uncontract_nbs <- function (nbs, graph, graph_c) {

    edge_map <- duplicated_edge_map (graph_c)
    graph <- duplicate_graph (graph)

    edges <- lapply (nbs$edges, function (i) {

        expand_edges (i, edge_map)
                            })

    nbs$edges <- edges

    return (nbs)
}
