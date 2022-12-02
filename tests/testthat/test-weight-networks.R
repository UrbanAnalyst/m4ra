
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

Sys.setenv ("M4RA_CACHE_DIR" = tempdir ())

test_that ("weight networks", {

    flist <- list.files (
        tempdir (),
        pattern = "^m4ra\\-",
        full.names = TRUE
    )
    chks <- "not null"
    if (length (flist) > 0L) {
        chks <- tryCatch (
            file.remove (flist),
            error = function (e) NULL
        )
    }

    skip_if (is.null (chks)) # shouldn't happen

    expect_message (
        f <- m4ra_weight_networks (m4ra_hampi, city = "hampi", quiet = FALSE),
        "Weighting network with"
    )

    expect_type (f, "character")
    expect_length (f, 24L)
    expect_true (all (file.exists (f)))
})
