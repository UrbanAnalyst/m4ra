
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

Sys.setenv ("M4RA_CACHE_DIR" = tempdir ())

test_that ("cache dir", {

    expect_silent (
        d <- m4ra_cache_dir ()
    )
    expect_true (dir.exists (d))
    expect_equal (d, tempdir ())
})
