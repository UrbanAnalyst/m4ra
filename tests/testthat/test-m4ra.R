
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

Sys.setenv ("M4RA_CACHE_DIR" = tempdir ())

test_that ("m4ra errors", {

    expect_error (m4ra (),
        "argument \"net\" is missing, with no default")
    expect_error (m4ra (net = 1L),
        "Assertion on \'net\' failed: Must inherit from class \'osmdata_sc\'")
    net <- structure (1L, class = "osmdata_sc")
    expect_error (m4ra (net = net),
        "Assertion on \'from\' failed: Must be of type \'character\', not \'NULL\'")
    expect_error (m4ra (net = net, from = "a"),
        "Unknown class")
})

test_that ("m4ra function", {

    net <- m4ra_hampi
    net_w <- dodgr::weight_streetnet (net, wt_profile = "foot")
    v <- dodgr::dodgr_vertices (net_w)
    set.seed (1L)
    from <- sample (v$id, size = 10L)

    #expect_silent ( # fstcore produces startup messages
        out <- m4ra (net, from = from)
    #)
})
