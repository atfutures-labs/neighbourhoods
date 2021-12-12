
#' Find candidate low-traffic-neighbourhoods in an input street network.
#'
#' @param network Street network in \pkg{silicate} `sc` format, extracted with
#' \pkg{dodgr} function, `dodgr_streetnet_sc`.
#' @param popdens Path to local population density file covering region of street
#' network, and in `geotiff` format.
#' @return A `data.frame` of candidate low-traffic neighbourhoods.
#' @export
neighbourhoods <- function (network, popdens) {

    cli::cli_h1 ("neighbourhoods")

    dodgr::dodgr_cache_off ()

    cli::cli_alert_info ("Weighting network for routing ... ")
    net <- dodgr::weight_streetnet (network, wt_profile = "motorcar")
    cli::cli_alert_success ("Weighted network for routing")
    net <- net [net$component == 1, ]
    net$flow <- 1
    cli::cli_alert_info ("Contracting network ... ")
    netc <- dodgr::dodgr_contract_graph (net)
    cli::cli_alert_success ("Calculated contracted network")
    netc$flow <- 1

    cli::cli_alert_info ("Calculating network centrality ... ")
    netc <- dodgr::dodgr_centrality (netc, contract = FALSE)
    cli::cli_alert_success ("Calculated network centrality")
    net <- dodgr::dodgr_uncontract_graph (netc) # adds centrality to original graph
    x <- dodgr::merge_directed_graph (netc)

    cli::cli_alert_info ("Extracting network cycles ... ")
    paths <- network_cycles (x) # 2-3 s
    cli::cli_alert_success ("Extracted network cycles")
    cli::cli_alert_info ("Identifying adjacent cycles ... ")
    nbs <- adjacent_cycles (paths) # 1-2 s
    cli::cli_alert_success ("Identifyed adjacent cycles")

    cli::cli_alert_info ("Caclulating additional cycle data ... ")
    nbs <- nbs_add_data (nbs, paths, net, netc, popdens)
    cli::cli_alert_success ("Caclulated additional cycle data")

    # remove zero centr_in rows:
    nbs <- nbs [which (nbs$centr_mn_in > 0), ]
    # and trim down to only certain types of highway
    nbs <- nbs [-grep ("^primary|^trunk|^service", nbs$hw_shared), ]
    # then remove any where from or to are lower roads than shared:
    hw_seq <- c ("living_street", "unclassified", "residential",
                 "tertiary", "tertiary_link",
                 "secondary", "secondary_link",
                 "primary", "primary_link",
                 "service", "trunk", "trunk_link", "motorway")
    hw_seq <- data.frame (shared = match (nbs$hw_shared, hw_seq),
                          from = match (nbs$hw_from, hw_seq),
                          to = match (nbs$hw_to, hw_seq))
    nbs <- nbs [which (hw_seq$shared < hw_seq$from &
                       hw_seq$shared < hw_seq$to), ]

    return (nbs)
}
