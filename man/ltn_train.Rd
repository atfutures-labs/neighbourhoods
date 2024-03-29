% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ltn-score.R
\name{ltn_train}
\alias{ltn_train}
\title{Train a prediction model to score LTNs from a sample of size, `n`.}
\usage{
ltn_train(nbs, n = 100, dmax = 10000)
}
\arguments{
\item{nbs}{Output of main \link{neighbourhoods} function.}

\item{n}{Size of sample to use in training and predicting data sets.}

\item{dmax}{Maximal distance in metres around neighbourhood to use to
generate scores.}
}
\value{
A trained model which can be used to predict additional LTN scores.
}
\description{
Train a prediction model to score LTNs from a sample of size, `n`.
}
