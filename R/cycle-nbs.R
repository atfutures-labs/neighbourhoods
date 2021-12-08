
#' Construct adjacency matrix of neighbourhood cycles
#'
#' @param cycles List of cycles obtained from \link{network_cycles}.
#' @return A `data.frame` of three columns:
#' \enumerate{
#' \item from - cycle from which connection is made
#' \item to - cycle to which connection is made
#' \edges - List-column of all shared edges between (from, to) pair.
#' }
#' @export
adjacent_cycles <- function (cycles) {

    paths <- lapply (seq_along (cycles), function (i)
                     cbind (cycles [[i]], cycle = i))
    paths_df <- do.call (rbind, paths) %>%
        dplyr::select (edge_, centrality, cycle) %>%
        dplyr::mutate (edge_ = gsub ("\\_rev$", "", edge_))

    # dplyr here is around 10 times slower
    nbs <- lapply (unique (paths_df$cycle), function (i) {

        index_in <- which (paths_df$cycle == i)
        index_out <- which (!seq (nrow (paths_df)) %in% index_in)
        e_i <- paths_df$edge_ [index_in] # all edges in cycle i
        e_j <- paths_df [index_out, ]
        e_j <- e_j [which (e_j$edge_ %in% e_i), ] # all shared edges from other cycles

        if (nrow (e_j) == 0L)
           return (NULL)

        res <- lapply (split (e_j, f = as.factor (e_j$cycle)),
                      function (i)
                      list (cycle = i$cycle [1],
                            edges = i$edge_))
        edges <- lapply (res, function (i) i$edges)
        cycle_to <- vapply (res, function (i) i$cycle,
                            integer (1))

        res <- data.frame (from = i,
                           to = cycle_to,
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

    edges <- cpp_expand_edges (nbs$edges, edge_map, paths_are_list = TRUE)

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

        p1 <- paths_exp [[nbs$from [i] ]]
        p2 <- paths_exp [[nbs$to [i] ]]
        # nbs$edges is a list-col:
        i1 <- which (!p1$edge_ %in% nbs$edges [[i]])
        i2 <- which (!p2$edge_ %in% nbs$edges [[i]])

        d1 <- sum (p1$d [i1], na.rm = TRUE)
        d2 <- sum (p2$d [i2], na.rm = TRUE)

        # (median, mean, max) of distance-weighted centrality
        one_centr <- function (centr, d) {
            centr_med <- centr_mn <- centr_max <- NA
            centr_d <- centr * d / sum (d)
            if (length (centr [which (!is.na (centr))]) > 0L) {
               centr_med <- stats::median (centr_d, na.rm = TRUE)
               centr_mn <- mean (centr_d, na.rm = TRUE)
               centr_max <- max (centr_d, na.rm = TRUE)
            }
            c (centr_med, centr_mn, centr_max)
        }
        c1 <- one_centr (p1$centrality [i1], p1$d [i1])
        c2 <- one_centr (p2$centrality [i2], p2$d [i2])

        p <- rbind (p1, p2)
        index_out <- which (!p$edge_ %in% nbs$edges [[i]])
        index_in <- which (p$edge_ %in% nbs$edges [[i]])
        c_in <- one_centr (p$centrality [index_in], p$d [index_in])
        c_out <- one_centr (p$centrality [index_out], p$d [index_out])

        c (d_in = sum (p$d [index_in]),
           d_out = sum (p$d [index_out]),
           centr_med_in = c_in [1],
           centr_mn_in = c_in [2],
           centr_max_in = c_in [3],
           centr_med_out = c_out [1],
           centr_mn_out = c_out [2],
           centr_max_out = c_out [3],
           centr_med_from = c1 [1],
           centr_mn_from = c1 [2],
           centr_max_from = c1 [3],
           centr_med_to = c2 [1],
           centr_mn_to = c2 [2],
           centr_max_to = c2 [3])
    }, numeric (14))

    extra_dat <- t (extra_dat)

    return (cbind (nbs, extra_dat))
}
