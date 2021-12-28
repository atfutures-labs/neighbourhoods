
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
ltn_score <- function (nbs, index, dmax = 10000) {

    scores <- pbapply::pblapply (index, function (i) cut_nbs (nbs, i, dmax = dmax))
    scores <- data.frame (do.call (rbind, scores))

    cbind (nbs$nbs [index, ], scores)
}

#' Train a prediction model to score LTNs from a sample of size, `n`.
#'
#' @inheritParams ltn_score
#' @param n Size of sample to use in training and predicting data sets.
#' @return A trained model which can be used to predict additional LTN scores.
#' @export
ltn_train <- function (nbs, n = 100, dmax = 10000) {
}
