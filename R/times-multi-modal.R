
#' Calculate matrix of pair-wise travel times between points using multiple
#' modes of transport.
#'
#' @param initial_mode Initial mode of transport from origin points towards
#' public transport stop.
#' @param from List of OSM vertex IDs from which to calculate total multi-modal
#' travel times. These must be vertices from the largest connected component of
#' the contracted graph.
#' @inheritParams m4ra_prepare_data
#' @family main
#' @export
m4ra_times_multi_mode <- function (net_sc = NULL,
                                   gtfs = NULL,
                                   city_name = NULL,
                                   day = NULL,
                                   start_time_limits = NULL,
                                   initial_mode = "foot",
                                   final_mode = "foot",
                                   from = NULL,
                                   fast = FALSE,
                                   n_closest = 10L,
                                   quiet = FALSE) {

    files <- m4ra_prepare_data (
        net_sc = net_sc,
        gtfs = gtfs,
        city_name = city_name,
        day = day,
        start_time_limits = start_time_limits,
        final_mode = final_mode,
        fast = fast,
        n_closest = n_closest,
        quiet = quiet
    )

    graph_c <- m4ra_load_cached_network (
        city = city_name,
        mode = initial_mode,
        contracted = TRUE
    )
    graph_c <- graph_c [graph_c$component == 1L, ]
    v <- dodgr::dodgr_vertices (graph_c)
    if (!all (from %in% v$id)) {
        stop (
            "Not all specified 'from' vertices are part of the ",
            "largest connected component of the street network."
        )
    }

    stops <- m_readRDS (gtfs)$stops
    f <- grep ("gtfs\\-.*[0-9]{5}\\-[0-9]{5}\\.Rds$", files, value = TRUE)
    gtfs_mat <- m_readRDS (f)
    f <- grep ("gtfs\\-to\\-net", files, value = TRUE)
    gtfs_to_net <- m_readRDS (f)


    # need times to all vertices to extract overall minimal time from initial
    # mode vs GTFS-routed modes at end. The times here are then reduced to times
    # to the GTFS stops by matching vertices afterward.
    times <- m4ra_times_single_mode (graph_c, from = from)
    to <- v$id [dodgr::match_points_to_verts (
        v, stops [, c ("stop_lon", "stop_lat")])]
    times_to_gtfs <- times [, match (to, colnames (times)), drop = FALSE]

    # Then convert initial times to nearest GTFS stops to times through entire
    # GTFS network to all termimal network vertices.
    times [is.na (times)] <- -1
    gtfs_mat [is.na (gtfs_mat)] <- -1

    res <- rcpp_add_net_to_gtfs (
        times_to_gtfs,
        gtfs_mat,
        gtfs_to_net$index,
        gtfs_to_net$d,
        nrow (v)
    )

    maxr <- rcpp_matrix_max (res)
    res [res == maxr] <- NA

    rownames (res) <- from
    colnames (res) <- v$id

    # At that stage, `res` holds the fastest values routed through the GTFS
    # network. They just then have to be compared with the previously-calculated
    # times to all vertices using "initial_mode".

    res <- rcpp_min_from_two_matrices (res, times)

    return (res)
}
