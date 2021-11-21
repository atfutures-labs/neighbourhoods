
#' Construct adjacency matrix of neighbourhood cycles
#'
#' @param cycles List of cycles obtained from \link{network_cycles}.
#' @return A `data.frame` of three columns:
#' \enumerate{
#' \item from - cycle from which connection is made
#' \item to - cycle to which connection is made
#' }
#' @export
adjacent_cycles <- function (cycles) {

    #measure <- match.arg (measure, c ("median", "mean", "max", "min"))
    measure <- "max"

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

        # Don't include centrality in output, as better versions are not
        # provided through `nbs_add_data()`.
        res <- data.frame (from = i,
                          to = res [, 1],
                          #centrality = res [, 2],
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

    nbs$edges <- I (edges)

    return (nbs)
}

#' Add centrality and approximate area to 'nbs' data.
#'
#' Approxmiate area because it calcualtes planar areas from geodesic
#' coordinates, but plenty near enough for present purposes.
#'
#' @return Modified version of `nbs` with additional columns of areas for each
#' "from" and "to" neighbourhood, along with various measures of centrality
#' outside and along the shared boundaries.
#' @noRd
nbs_add_data <- function (nbs, paths, graph, graph_c) {

    paths_exp <- uncontract_cycles (paths, graph, graph_c)

    sf::sf_use_s2 (FALSE)
    one_area <- function (p) {
        xy <- as.matrix (p [c (seq (nrow (p)), 1), c (".vx0_x", ".vx0_y")])
        p <- sf::st_sfc (sf::st_polygon (list (xy)), crs = 4326)
        sf::st_area (p)
    }
    a <- vapply (paths, function (p) one_area (p), numeric (1))
    nbs$area_from <- a [nbs$from]
    nbs$area_to <- a [nbs$to]

    nbs <- uncontract_nbs (nbs, graph, graph_c)

    extra_dat <- vapply (seq.int (nrow (nbs)), function (i) {

        p <- rbind (paths_exp [[nbs$from [i] ]],
                    paths_exp [[nbs$to [i] ]])
        # nbs$edges is a list-col:
        index_out <- which (!p$edge_ %in% nbs$edges [[i]])
        index_in <- which (p$edge_ %in% nbs$edges [[i]])

        c (d_in = sum (p$d [index_in]),
           d_out = sum (p$d [index_out]),
           centr_med_in = median (p$centrality [index_in], na.rm = TRUE),
           centr_mn_in = mean (p$centrality [index_in], na.rm = TRUE),
           centr_max_in = max (p$centrality [index_in], na.rm = TRUE),
           centr_med_out = median (p$centrality [index_out], na.rm = TRUE),
           centr_mn_out = mean (p$centrality [index_out], na.rm = TRUE),
           centr_max_out = max (p$centrality [index_out], na.rm = TRUE))
    }, numeric (8))

    extra_dat <- t (extra_dat)

    return (cbind (nbs, extra_dat))
}
