test_that ("gtfs travel times", {

    z <- gtfsrouter::berlin_gtfs_to_zip ()
    suppressWarnings (
        gtfs <- gtfsrouter::extract_gtfs (z)
    )
    gtfs <- gtfsrouter::gtfs_timetable (gtfs, day = "Monday")

    Sys.setenv ("M4RA_NUM_CORES" = 1L)
    start_time_limits <- 12:13 * 3500
    res <- m4ra_gtfs_traveltimes (gtfs, start_time_limits = start_time_limits)

    expect_type (res, "list")
    expect_length (res, 3L)
    expect_identical (names (res), c ("duration", "ntransfers", "intervals"))

    tt <- res$duration
    expect_type (tt, "integer")
    expect_equal (nrow (tt), ncol (tt))
    expect_true (length (which (is.na (tt))) > 0)
    expect_true (length (which (!is.na (tt))) > 0)

    expect_identical (dim (tt), dim (res$ntransfers))

    file.remove (z)
})

test_that ("gtfs to graph fns", {

    z <- gtfsrouter::berlin_gtfs_to_zip ()
    suppressWarnings (
        gtfs <- gtfsrouter::extract_gtfs (z)
    )
    gtfs <- gtfsrouter::gtfs_timetable (gtfs, day = "Monday")

    npts <- 1e3L
    graph <- fake_network_data (gtfs, npts = npts)

    Sys.setenv ("M4RA_NUM_CORES" = 1L)
    start_time_limits <- 12:13 * 3500
    res <- m4ra_gtfs_traveltimes (gtfs, start_time_limits = start_time_limits)
    tmat_gtfs_intern <- res$duration

    tmat_gtfs_net <- m4ra_times_to_gtfs_stops (graph, gtfs = gtfs, city = "berlin", graph_to_gtfs = FALSE)
    tmat_net_gtfs <- m4ra_times_to_gtfs_stops (graph, gtfs = gtfs, city = "berlin", graph_to_gtfs = TRUE)

    nstops <- nrow (gtfs$stops)
    nverts <- nrow (dodgr::dodgr_vertices (graph))
    expect_equal (nverts, npts)

    expect_identical (dim (tmat_gtfs_intern), c (nstops, nstops))
    expect_identical (dim (tmat_gtfs_net), c (nstops, nverts))
    expect_identical (dim (tmat_net_gtfs), c (nverts, nstops))
})
