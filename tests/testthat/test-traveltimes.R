
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

test_that ("save times to local cache", {

    net <- dodgr::weight_streetnet (
        m4ra_hampi,
        wt_profile = "bicycle",
        turn_penalty = TRUE
    )

    v <- dodgr::dodgr_vertices (net)
    set.seed (1L)
    from <- sample (v$id, size = 10)

    path <- file.path (tempdir (), "m4ra")
    if (dir.exists (path)) {
        unlink (path, recursive = TRUE)
    }
    dir.create (path, recursive = TRUE)
    fnames <- m4ra_times (net, from = from, path = path)

    expect_type (fnames, "character")
    expect_equal (length (fnames), length (from))
    expect_true (all (file.exists (fnames)))
    expect_true (all (grepl ("m4ra\\_from\\_", fnames)))
    fnames_from_id <- gsub ("^m4ra\\_from\\_", "", basename (fnames))
    expect_identical (fnames_from_id, from) # order is the same!!

    # confirm that the saved version is numerically close to version calculated
    # directly:
    d1 <- as.numeric (readLines (fnames [1]))
    d1 [d1 < 0] <- NA
    d2 <- m4ra_times (net, from = from [1])
    d2 <- as.numeric (d2)

    d1_index <- which (!is.na (d1))
    d2_index <- which (!is.na (d2))
    expect_identical (d1_index, d2_index)

    d1 <- d1 [d1_index]
    d2 <- d2 [d2_index]
    expect_true (max (abs (d1 - d2)) < 0.1)
})
