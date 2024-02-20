test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

Sys.setenv ("M4RA_CACHE_DIR" = fs::path_temp ())

test_that ("weight networks", {

    flist <- fs::dir_ls (
        fs::path_temp (),
        regexp = "^m4ra\\-"
    )
    chks <- "not null"
    if (length (flist) > 0L) {
        chks <- tryCatch (
            fs::file_delete (flist),
            error = function (e) NULL
        )
    }

    skip_if (is.null (chks)) # shouldn't happen

    expect_message (
        f <- m4ra_weight_networks (m4ra_hampi, city = "hampi", quiet = FALSE),
        "Weighting network with"
    )

    expect_type (f, "character")
    expect_length (f, 18L)
    expect_true (all (fs::file_exists (f)))
})
