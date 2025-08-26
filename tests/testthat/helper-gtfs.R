# fake network data from gtfs feed
fake_network_data <- function (gtfs, npts = 1e3) {

    stops <- gtfs$stops [, c ("stop_lat", "stop_lon")]
    xlim <- range (stops$stop_lat)
    ylim <- range (stops$stop_lon)

    set.seed (1)
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

    graph$d <- sqrt ((graph$.vx1_x - graph$.vx0_x)^2 +
        (graph$.vx1_y - graph$.vx0_y)^2)
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
