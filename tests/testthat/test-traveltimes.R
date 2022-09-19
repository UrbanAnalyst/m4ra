
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

test_that ("times without turn penalty", {

    net <- dodgr::weight_streetnet (
        m4ra_hampi,
        wt_profile = "bicycle",
        turn_penalty = FALSE
    )
    expect_s3_class (net, "dodgr_streetnet_sc")
    expect_equal (attr (net, "turn_penalty"), 0)

    tmat <- m4ra_times (net)
    expect_type (tmat, "double")

    v <- dodgr::dodgr_vertices (net)
    expect_equal (nrow (v), nrow (tmat))
    expect_equal (dim (tmat), rep (nrow (v), 2))
    expect_true (all (rownames (tmat) %in% v$id))
    expect_true (all (colnames (tmat) %in% v$id))
})

test_that ("times with turn penalty", {

    # These tests are identical, but calls different internal code which expands
    # the network out to include compound junctions before removing these
    # additional graph rows at the end to reduce to the original dimensions.

    net <- dodgr::weight_streetnet (
        m4ra_hampi,
        wt_profile = "bicycle",
        turn_penalty = TRUE
    )
    expect_s3_class (net, "dodgr_streetnet_sc")
    expect_true (attr (net, "turn_penalty") > 0)

    tp <- get_turn_penalty (net)
    expect_type (tp, "double")
    expect_true (tp > 0)

    tmat <- m4ra_times (net)
    expect_type (tmat, "double")

    v <- dodgr::dodgr_vertices (net)
    expect_equal (nrow (v), nrow (tmat))
    expect_equal (dim (tmat), rep (nrow (v), 2))
    expect_true (all (rownames (tmat) %in% v$id))
    expect_true (all (colnames (tmat) %in% v$id))
})
