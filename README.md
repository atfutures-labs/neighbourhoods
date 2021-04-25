---
output:
  md_document:
    variant: markdown\_github
  rmarkdown::html_vignette:
    self_contained: no
---

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R build
status](https://github.com/ATFutures-labs/neighbourhoods/workflows/R-CMD-check/badge.svg)](https://github.com/ATFutures-labs/neighbourhoods/actions)
[![codecov](https://codecov.io/gh/ATFutures-labs/neighbourhoods/branch/master/graph/badge.svg)](https://codecov.io/gh/ATFutures-labs/neighbourhoods)
[![Project Status: Concept - Minimal or no implementation has been done
yet.](http://www.repostatus.org/badges/0.1.0/concept.svg)](http://www.repostatus.org/#concept)
<!-- badges: end -->

# Network Neighbourhoods

Algorithm for efficient identification of enclosed cyclic neighbourhoods
within networks, and functions for aggregating neighbourhoods according
to network properties. One example application is automated
identification of so-called “Low-Traffic Neighbourhoods” (LTNs), which
can be identified through a dedicated function, `ltns()`. These are
identified by first decomposing a network into its constituent
“neighbourhoods” of enclosed polygons. Traffic flows along each edge may
then be estimated with the [`dodgr_centrality()`
function](https://atfutures.github.io/dodgr/reference/dodgr_centrality.html)
of the [`dodgr` package](https://atfutures.github.io/dodgr), allowing
neighbourhoods to be successively merged across shared low-traffic edges
until a specified stopping criterion, such as total number of
neighbourhoods, has been achieved.
