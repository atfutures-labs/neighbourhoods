
#' Convert cycles created on contracted graph back to equivalent uncontracted
#' cycles.
#'
#' @param paths List of cycle paths as a result of \link{network_cycles}.
#' @param graph Full, non-contracted graph.
#' @param graph_c Contracted graph resulting from call to
#' `dodgr_contract_graph`.
#' @return Equivalent list of `paths`, with each path expanded out to full
#' edges in original, non-contracted graph.
#' @export
uncontract_cycles <- function (paths, graph, graph_c) {

    edge_map <- duplicated_edge_map (graph_c)

    graph <- duplicate_graph (graph)

    edges_expanded <- cpp_expand_edges (paths, edge_map, paths_are_list = FALSE)

    graph_exp <- lapply (edges_expanded, function (i) {

        graph [match (i, graph$edge_), ]
    })

    return (graph_exp)
}

#' Load cached edge_map and duplicate all edges in reversed form.
#'
#' @noRd
duplicated_edge_map <- function (graph_c) {

    # load edge_map of contracted graph:
    hash_c <- attr (graph_c, "hashc")
    if (is.null (hash_c)) {
        stop ("Edge map of graph can not be recovered; ",
              "function must be run in same R session as graph was created.",
              call. = FALSE)
    }

    flist <- list.files (tempdir (), pattern = hash_c, full.names = TRUE)
    emap <- grep ("edge\\_map", flist, value = TRUE)
    if (length (emap) != 1L) {
        stop ("Edge map of graph can not be recovered; ",
              "function must be run in same R session as graph was created.",
              call. = FALSE)
    }
    emap <- readRDS (emap)

    # duplicate in reversed form:
    emap_rev <- split (emap, f = factor (emap$edge_new))
    emap_rev <- lapply (emap_rev, function (i) {
                            i$edge_old <- rev (i$edge_old)
                            return (i)  })
    emap_rev <- do.call (rbind, emap_rev)
    emap_rev$edge_new <- paste0 (emap_rev$edge_new, "_rev")
    emap <- rbind (emap, emap_rev)
    rownames (emap) <- NULL

    return (emap)
}

#' Duplicate all rows of graph in reversed form.
#'
#' @noRd
duplicate_graph <- function (graph) {

    graph_rev <- graph

    .vx0 <- graph_rev$.vx0
    graph_rev$.vx0 <- graph_rev$.vx1
    graph_rev$.vx1 <- .vx0

    .vx0_x <- graph_rev$.vx0_x
    graph_rev$.vx0_x <- graph_rev$.vx1_x
    graph_rev$.vx1_x <- .vx0_x
    .vx0_y <- graph_rev$.vx0_y
    graph_rev$.vx0_y <- graph_rev$.vx1_y
    graph_rev$.vx1_y <- .vx0_y

    index1 <- grep ("\\_rev$", graph_rev$edge_)
    index2 <- seq (nrow (graph)) # edges without "_rev$"
    if (length (index1) > 0L) {
        index2 <- index2 [-index1]
    }
    graph_rev$edge_ [index1] <- gsub ("\\_rev$", "", graph_rev$edge_ [index1])
    graph_rev$edge_ [index2] <- paste0 (graph_rev$edge_ [index2], "_rev")

    graph_rev <- graph_rev [which (!graph_rev$edge_ %in% graph$edge_), ]

    rbind (graph, graph_rev)
}
