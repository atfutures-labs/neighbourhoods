#' ltn
#'
#' Convert a street network into a list of Low Traffic Neighbourhoods (LTNs).
#'
#' @param net Street network to be analysed
#' @param verts Result of call to `dodgr_centrality` with `edges = FALSE`
#' and so returning vertex-based centrality.
#' @return List of sub-networks, each of which represents a candidate LTN
#' @export
ltn <- function (net, verts) {
    #test (net, verts)
}
