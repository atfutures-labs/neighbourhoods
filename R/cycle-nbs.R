
#' Construct adjacency matrix of neighbourhood cycles
#'
#' @param cycles List of cycles obtained from \link{network_cycles}.
#' @return A `data.frame` of three columns:
#' \enumerate{
#' \item from - cycle from which connection is made
#' \item to - cycle to which connection is made
#' \item centrality - Median centrality of edges connecting those two cycles
#' }
#' @export
adjacent_cycles <- function (cycles) {

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
                       res <- t (vapply (split (e_j, f = as.factor (e_j$cycle)), function (i)
                                         c (i$cycle [1], median (i$centrality)),
                                         numeric (2)))
                       cbind (i, res)
                     })
    nbs <- data.frame (do.call (rbind, nbs), row.names = NULL)
    names (nbs) <- c ("from", "to", "centrality")
    nbs <- nbs [which (nbs$from != nbs$to), ]

    return (nbs)
}
