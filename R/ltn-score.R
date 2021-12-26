
#' Score an LTN formed by blocking one street segment between two adjacent
#' neighbourhoods.
#'
#' @param nbs Output of main \link{neighbourhoods} function.
#' @param index Index into rows of `nbs$nbs` specifying which pairs of adjacent
#' neighbourhoods are to be scored.
#' @param dmax Maximal distance in metres around neighbourhood to use to
#' generate scores.
#' @return Modified version of `nbs$nbs` from input parameter, reduced to only
#' those neighbour pairs specified in `index`, and with additional column,
#' `pop_decr_in` and `pop_incr_out` specifying absolute decreases within
#' and increases surrounding proposed LTN.
ltn_score <- function (nbs, index, dmax) {
}
