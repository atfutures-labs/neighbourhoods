
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

    edges_expanded <- lapply (paths, function (p) {

        expand_edges (p$edge_, edge_map)
    })

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
    graph_rev$edge_ <- paste0 (graph_rev$edge_, "_rev")

    .vx0_x <- graph_rev$.vx0_x
    graph_rev$.vx0_x <- graph_rev$.vx1_x
    graph_rev$.vx1_x <- .vx0_x
    .vx0_y <- graph_rev$.vx0_y
    graph_rev$.vx0_y <- graph_rev$.vx1_y
    graph_rev$.vx1_y <- .vx0_y

    rbind (graph, graph_rev)
}

#' Expand one set of edges out to original (uncontracted) graph edges.
#'
#' @param edges A character vector of edges including contracted edges.
#' @param edge_map The duplicated version returned from 'duplicate_edge_map'
#' @noRd
expand_edges <- function (edges, edge_map) {

    expands <- which (edges %in% edge_map$edge_new)
    edge_new <- edges [expands]
    not_expands <- which (!edges %in% edge_map$edge_new)

    edge_old <- lapply (expands, function (i)
                        edge_map$edge_old [which (edge_map$edge_new == edges [i])])
    n <- vapply (edge_old, length, integer (1))

    index <- rep (1, length (edges))
    index [expands] <- n
    index <- rep (seq (length (edges)), times = index)

    # build vector of expanded edges of cycle. Non-expanded edges retain
    # original IDs, so only need to re-map expanded edges
    edges <- edges [index]
    edges [which (index %in% expands)] <- unlist (edge_old)

    return (edges)
}
