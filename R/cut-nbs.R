
#' Use result of \link{neighbourhoods} function to make and score an LTN
#'
#' @param nbs Results of \link{neighbourhoods} function.
#' @param i Index of which neighbour pair is to be cut.
#' @export
cut_nbs <- function (nbs, i) {

    edges_in <- nbs$nbs$edges [[i]]
    index <- match (edges_in, nbs$network$edge_)
    d <- cumsum (nbs$network$d [index])
    cut_point <- which.min (abs (d - max (d) / 2))
    cut_edge <- edges_in [cut_point]
    cut_index <- index [match (cut_edge, edges_in)]
    v0 <- nbs$network$.vx0 [cut_index]
    v1 <- nbs$network$.vx1 [cut_index]
    dupl <- which (nbs$network$.vx0 == v1 & nbs$network$.vx1 == v0)
    if (length (dupl) > 0L) {
        cut_edge <- c (cut_edge, nbs$network$edge_ [dupl])
        cut_index <- c (cut_index, dupl)
    }

    edges_out <- unlist (c (nbs$edges [nbs$nbs$from [i]],
                            nbs$edges [nbs$nbs$to [i]]))
    edges_out <- edges_out [which (!edges_out %in% edges_in)]

    index <- cut_networks (nbs, cut_index, dlim = 10000)
    net_full <- nbs$network [index, ]
    cut_index <- match (cut_edge, nbs$network$edge_)
    net_cut <- net_full [-cut_index, ]

    centr <- compare_centrality (net_full, net_cut, edges_in, edges_out,
                                 cut_index)
    pop <- as.integer ((nbs$nbs$area_from [i] + nbs$nbs$area_to [i]) *
        (nbs$nbs$popdens_from [i] + nbs$nbs$popdens_to [i])) / 1e6
    pop_decr_in <- nbs$nbs$d_in [i] * pop * centr [["centr_decr_in"]]
    pop_incr_out <- nbs$nbs$d_out [i] * pop * centr [["centr_incr_out"]]

    return (c (pop_decr_in = pop_decr_in, pop_incr_out = pop_incr_out))
}

#' Cut networks down to a bbox within specified metres of specified point.
#' @noRd
cut_networks <- function (nbs, cut_index, dlim = 10000) {

    net_full <- nbs$network
    x0 <- mean (c (net_full$.vx0_x [cut_index],
                   net_full$.vx1_x [cut_index]))
    y0 <- mean (c (net_full$.vx0_y [cut_index],
                   net_full$.vx1_y [cut_index]))

    bb_dlim <- function (lim, x0, y0, dlim = 20000) {
        xy <- cbind (x = c (x0 - lim, x0 + lim),
                     y = c (y0 - lim, y0 + lim))
        abs (geodist::geodist (xy, measure = "haversine") [1, 2] - dlim)
    }

    op <- optimize (bb_dlim, c (0, 1), x0 = x0, y0 = y0, dlim = dlim)
    bb <- data.frame (cbind (x = c (x0 - op$minimum, x0 + op$minimum),
                             y = c (y0 - op$minimum, y0 + op$minimum)))

    index <- which (((net_full$.vx0_x > bb$x [1] &
                      net_full$.vx0_x < bb$x [2]) |
                     (net_full$.vx1_x > bb$x [1] &
                      net_full$.vx1_x < bb$x [2])) &
                    ((net_full$.vx0_y > bb$y [1] &
                      net_full$.vx0_y < bb$y [2]) |
                     (net_full$.vx1_y > bb$y [1] &
                      net_full$.vx1_y < bb$y [2])))

    return (index)
}

compare_centrality <- function (net_full, net_cut, edges_in, edges_out,
                                cut_index) {

    net_full$centrality <- net_cut$centrality <- NULL
    net_full <- dodgr::dodgr_centrality (net_full)
    net_cut <- dodgr::dodgr_centrality (net_cut)

    index_cut <- match (edges_in, net_cut$edge_)
    index_cut <- index_cut [which (!is.na (index_cut))]
    centr_cut_in <- mean (net_cut$centrality [index_cut])

    index_full <- match (edges_in, net_full$edge_)
    centr_full_in <- mean (net_full$centrality [index_full])

    centr_decr_in <- 1 - centr_cut_in / centr_full_in

    edges_not_in_net <- edges_out [which (!edges_out %in% net_full$edge_)]
    edges_out <- edges_out [which (edges_out %in% net_full$edge_)]
    if (length (edges_not_in_net) > 0L) {
        index <- grepl ("\\_rev$", edges_not_in_net)
        edges_not_in_net [which (index)] <- gsub ("\\_rev$", "", edges_not_in_net [which (index)])
        edges_not_in_net [which (!index)] <- paste0 (edges_not_in_net [which (!index)], "_rev")
        edges_not_in_net <- edges_not_in_net [which (edges_not_in_net %in% net_full$edge_)]
    }
    edges_out <- c (edges_out, edges_not_in_net)

    net_full <- net_full [-cut_index, ]
    index <- match (edges_out, net_full$edge_)
    i0 <- which (!is.na (net_full$centrality [index]) & net_full$centrality[index] == 0)
    net_full$centrality [index [i0]] <- NA
    centr_incr_out <- mean (net_cut$centrality [index] /
                            net_full$centrality [index], na.rm = TRUE) - 1

    return (c (centr_decr_in = centr_decr_in, centr_incr_out = centr_incr_out))
}
