
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

Sys.setenv ("M4RA_CACHE_DIR" = tempdir ())

test_that ("weight networks", {

    expect_message (
        f <- m4ra_weight_networks (m4ra_hampi, quiet = FALSE),
        "Weighting network with"
    )

    expect_type (f, "character")
    expect_length (f, 2L)
    expect_true (all (file.exists (f)))
})
