
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
m4ra_times_multi_mode <- function (net_sc = NULL, gtfs = NULL, city_name = NULL,
                                   day = NULL, start_time_limits = NULL,
                                   initial_mode = "foot", final_mode = "foot",
                                   from = NULL,
                                   fast = FALSE, n_closest = 10L, quiet = FALSE) {

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

    graph <- m4ra_load_cached_network (city = city_name, mode = initial_mode)
    stops <- readRDS (gtfs)$stops
    f <- grep ("gtfs\\-.*[0-9]{5}\\-[0-9]{5}\\.Rds$", files, value = TRUE)
    gtfs_mat <- readRDS (f)
    f <- grep ("gtfs\\-to\\-net", files, value = TRUE)
    gtfs_to_net <- readRDS (f)

    graph_c <- dodgr::dodgr_contract_graph (graph)
    graph_c <- graph_c [graph_c$component == 1L, ]
    v <- dodgr::dodgr_vertices (graph_c)
    if (!all (from %in% v$id)) {
        stop (
            "Not all specified 'from' vertices are part of the ",
            "largest connected component of the street network."
        )
    }

    to <- v$id [dodgr::match_points_to_verts (
        v, stops [, c ("stop_lon", "stop_lat")])]
    times <- m4ra_times_single_mode (graph_c, from = from, to = to)
    n_closest <- update_n_closest (v, stops, n_closest = 10L)

    # Then convert initial times to nearest GTFS stops to times through entire
    # GTFS network to all termimal network vertices:
    res <- rcpp_add_net_to_gtfs (times, gtfs_mat, gtfs_to_net$index, gtfs_to_net$d, nrow (v))
    maxr <- rcpp_matrix_max (res)
    res [res == maxr] <- NA

    rownames (res) <- from
    colnames (res) <- v$id

    return (res)
}

#' Calculate matrix of pair-wise travel times between points using multiple
#' modes of transport.
#'
#' Alias for \link{m4ra_times_multi_mode}
#' @inherit m4ra_times_multi_mode
#' @family main
#' @export
m4ra_times_multi_modal <- function (net_sc = NULL, gtfs = NULL, city_name = NULL,
                                   day = NULL, start_time_limits = NULL,
                                   initial_mode = "foot", final_mode = "foot",
                                   from = NULL,
                                   fast = FALSE, n_closest = 10L, quiet = FALSE) {

    m4ra_times_multi_mode (
        net_sc = net_sc,
        gtfs = gtfs,
        city_name = city_name,
        day = day,
        start_time_limits = start_time_limits,
        initial_mode = initial_mode,
        final_mode = final_mode,
        from = from,
        fast = fast,
        n_closest = n_closest,
        quiet = quiet
    )
}
