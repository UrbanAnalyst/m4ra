
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

# fake network data from gtfs feed
fake_network_data <- function (gtfs, npts = 1e3) {

    stops <- gtfs$stops [, c ("stop_lat", "stop_lon")]
    xlim <- range (stops$stop_lat)
    ylim <- range (stops$stop_lon)

    set.seed (1)
    npts <- 1e3
    xy <- data.frame (
        x = runif (npts, min = xlim [1], max = xlim [2]),
        y = runif (npts, min = ylim [1], max = ylim [2])
    )

    nedges <- npts * 5
    graph <- data.frame (
        .vx0 = sample (seq (npts), nedges, replace = TRUE),
        .vx1 = sample (seq (npts), nedges, replace = TRUE),
        edge_ = seq (nedges)
    )
    graph$.vx0_x <- xy$x [graph$.vx0]
    graph$.vx0_y <- xy$y [graph$.vx0]
    graph$.vx1_x <- xy$x [graph$.vx1]
    graph$.vx1_y <- xy$y [graph$.vx1]

    graph$d <- sqrt ((graph$.vx1_x - graph$.vx0_x) ^ 2 +
        (graph$.vx1_y - graph$.vx0_y) ^ 2)
    graph$d_weighted <- graph$time <- graph$time_weighted <- graph$d

    graph_rev <- data.frame (
        .vx0 = graph$.vx1,
        .vx1 = graph$.vx0,
        edge_ = graph$edge_ + max (graph$edge_),
        .vx0_x = graph$.vx1_x,
        .vx0_y = graph$.vx1_y,
        .vx1_x = graph$.vx0_x,
        .vx1_y = graph$.vx0_y,
        d = graph$d,
        d_weighted = graph$d_weighted,
        time = graph$time,
        time_weighted = graph$time_weighted
    )
    graph <- rbind (graph, graph_rev)

    graph <- dodgr::dodgr_components (graph)
    graph <- graph [which (graph$component == 1L), ] # should be single-component

    graph$edge_ <- paste0 (graph$edge_)
    graph$.vx0 <- paste0 (graph$.vx0)
    graph$.vx1 <- paste0 (graph$.vx1)

    class (graph) <- c ("dodgr_streetnet_sc", class (graph))

    return (graph)
}

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
