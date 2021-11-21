
#' Convert cycles created on contracted graph back to equivalent uncontracted
#' cycles.
#'
#' @param paths List of cycle paths as a result of \link{network_cycles}.
#' @param graph Full, non-contracted graph.
#' @param graph_c Contracted graph resulting from call to `dodgr_contract_graph`.
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

    for (p in paths) {

        expands <- which (p$edge_ %in% emap$edge_new)
        edge_new <- p$edge_ [expands]
        not_expands <- which (!p$edge_ %in% emap$edge_new)
        n <- vapply (expands, function (i)
                     length (which (emap$edge_new == p$edge_ [i])),
                     integer (1))
        index <- rep (1, nrow (p))
        index [expands] <- n
        index <- rep (seq (nrow (p)), times = index)
        pnew <- p [index, ]

        not_expands <- which (index %in% not_expands)
        pnew$edge_ [not_expands] <- match (pnew$edge_ [not_expands], graph$edge_)

        edge_old <- emap$edge_old [which (emap$edge_new %in% p$edge_ [expands])]
        #match (edge_old, graph$edge_)
    }
}
