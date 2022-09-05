
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

Sys.setenv ("M4RA_CACHE_DIR" = tempdir ())

test_that ("m4ra function", {

    net <- m4ra_hampi
    net_w <- dodgr::weight_streetnet (net, wt_profile = "foot")
    v <- dodgr::dodgr_vertices (net_w)
    set.seed (1L)
    from <- sample (v$id, size = 10L)

    expect_silent (
        out <- m4ra (net, from = from)
    )
})
