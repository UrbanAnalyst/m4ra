
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

Sys.setenv ("M4RA_CACHE_DIR" = tempdir ())

test_that ("m4ra errors", {

    expect_error (
        m4ra_times_multi_mode (),
        "Assertion on 'city_name' failed: Must be of type 'character'"
    )
    expect_error (
        m4ra_times_multi_mode (net = 1L),
        "Assertion on 'net_sc' failed: Must be of type 'character', not 'integer'"
    )

    net <- m4ra_hampi
    net_path <- fs::path (fs::path_temp (), "hampi.Rds")
    saveRDS (net, net_path)
    expect_error (
        m4ra_times_multi_mode (net = net_path, from = "a", gtfs = 1L),
        "Assertion on 'gtfs' failed: Must be of type 'character', not 'integer'"
    )

    try (file.remove (net_path))
})

test_that ("m4ra function", {

    net <- m4ra_hampi
    net_path <- fs::path (fs::path_temp (), "hampi.Rds")
    if (!file.exists (net_path)) {
        saveRDS (net, net_path)
    }

    net_w <- dodgr::weight_streetnet (net, wt_profile = "foot")
    v <- dodgr::dodgr_vertices (net_w)
    set.seed (1L)
    from <- sample (v$id, size = 10L)

    # expect_silent ( # fstcore produces startup messages
    #    out <- m4ra_times_multi_mode (net_path, from = from)
    # )
    try (file.remove (net_path))
})
