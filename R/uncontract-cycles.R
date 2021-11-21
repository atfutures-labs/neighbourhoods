
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

    graph <- merge_directed_graph (graph)
    # also duplicate in reversed form:
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

    graph <- rbind (graph, graph_rev)

    edges_expanded <- lapply (paths, function (p) {

        expands <- which (p$edge_ %in% emap$edge_new)
        edge_new <- p$edge_ [expands]
        not_expands <- which (!p$edge_ %in% emap$edge_new)

        edge_old <- lapply (expands, function (i)
                    emap$edge_old [which (emap$edge_new == p$edge_ [i])])
        n <- vapply (edge_old, length, integer (1))

        index <- rep (1, nrow (p))
        index [expands] <- n
        index <- rep (seq (nrow (p)), times = index)

        # build vector of expanded edges of cycle. Non-expanded edges retain
        # original IDs, so only need to re-map expanded edges
        edges <- p$edge_ [index]
        edges [which (index %in% expands)] <- unlist (edge_old)

        return (edges)
    })

    graph_exp <- lapply (edges_expanded, function (i) {

        graph [match (i, graph$edge_), ]
    })

    return (graph_exp)
}
