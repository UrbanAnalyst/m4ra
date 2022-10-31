
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

test_that ("prepare data", {

    Sys.setenv ("M4RA_CACHE_DIR" = tempdir ())
    Sys.setenv ("M4RA_NUM_CORES" = 1L)

    z <- gtfsrouter::berlin_gtfs_to_zip ()
    gtfs <- gtfsrouter::extract_gtfs (z)
    gtfs_path <- file.path (tempdir (), "berlin-gtfs.Rds")
    saveRDS (gtfs, gtfs_path)

    # modify coordinates of hampi data to match GTFS coordinates:
    net_sc <- m4ra_hampi
    x0 <- min (gtfs$stops$stop_lon)
    xlim <- range (gtfs$stops$stop_lon)
    y0 <- min (gtfs$stops$stop_lat)
    ylim <- range (gtfs$stops$stop_lat)
    net_sc$vertex$x_ <- (net_sc$vertex$x_ - min (net_sc$vertex$x_)) /
        diff (range (net_sc$vertex$x_)) * diff (xlim) + x0
    net_sc$vertex$y_ <- (net_sc$vertex$y_ - min (net_sc$vertex$y_)) /
        diff (range (net_sc$vertex$y_)) * diff (ylim) + y0
    net_sc_path <- file.path (tempdir (), "berlin-sc.Rds")
    saveRDS (net_sc, net_sc_path)

    day <- "monday"
    start_time_limits <- 12:13 * 3500

    flist <- m4ra_prepare_data (
        net_sc = net_sc_path,
        gtfs = gtfs_path,
        city_name = "berlin",
        day = "mo",
        start_time_limits = 8:9 * 3600,
        final_mode = "foot",
        fast = FALSE,
        n_closest = 10L)

    expect_type (flist, "character")
    expect_true (all (file.exists (flist)))

    expect_length (flist, 6L)
    # 3 weighted networks:
    expect_length (grep ("foot|bicycle|motorcar", flist), 3L)
    # 1 GTFS source:
    expect_length (grep ("berlin\\-gtfs\\.Rds$", flist), 1L)
    # plus one GTFS travel time matrix, and one GTFS-to-final network time
    # matrix:
    expect_length (grep ("gtfs", flist), 3L)
})
