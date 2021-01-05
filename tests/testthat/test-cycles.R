
test_that("cycles", {

    library (dodgr)
    dodgr::dodgr_cache_off ()

    net <- dodgr::weight_streetnet (hampi_sc, wt_profile = "foot")
    net <- net [net$component == 1, ]
    netc <- dodgr::dodgr_contract_graph (net)
    netc$flow <- 1
    x <- dodgr::merge_directed_graph (netc)

    expect_message (paths <- ltn_cycles (x))
    expect_type (paths, "list")
    expect_equal (length (paths), 48)
})
