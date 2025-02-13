test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

Sys.setenv ("M4RA_CACHE_DIR" = fs::path_temp ())

test_that ("cache dir", {

    expect_silent (
        d <- m4ra_cache_dir ()
    )
    expect_true (fs::dir_exists (d))
    expect_equal (d, as.character (fs::path_temp ()))
})


skip_if (!test_all)

test_that ("cache dir", {

    d0 <- m4ra_cache_dir ()
    Sys.unsetenv ("M4RA_CACHE_DIR")
    d1 <- m4ra_cache_dir ()

    expect_length (d0, 1L)
    expect_length (d1, 1L)
    expect_true (d0 != d1)
    expect_true (d0 == fs::path_temp ())
    expect_true (d1 != fs::path_temp ())

    # unlink (d1, recursive = TRUE)
})
