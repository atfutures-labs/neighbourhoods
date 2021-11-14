
#' Convert cycles created on contracted graph back to equivalent uncontracted
#' cycles.
#'
#' @param paths List of cycle paths as a result of \link{network_cycles}.
#' @param graph Contracted graph resulting from call to `dodgr_contract_graph`.
#' @return Equivalent list of `paths`, with each path expanded out to full
#' edges in original, non-contracted graph.
#' @export
uncontract_cycles <- function (paths, graph) {

    # load edge_map of contracted graph:
    hash_c <- attr (graph, "hashc")
    if (is.null (hash_c)) {
        stop ("Edge map of graph can not be recovered; ",
              "function must be run in same R session as graph was created.",
              call. = FALSE)
    }

    flist <- list.files (tempdir (), pattern = hash_c, full.names = TRUE)
    emap <- grep ("edge\\_map", flist, value = TRUE)
    if (length (emap != 1L)) {
        stop ("Edge map of graph can not be recovered; ",
              "function must be run in same R session as graph was created.",
              call. = FALSE)
    }
    emap <- readRDS (emap)
}
