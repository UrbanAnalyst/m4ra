
#' Calculate matrix of pair-wise travel times between points using multiple
#' modes of transport.
#'
#' @param graph A `dodgr` network returned from the \link{weight_streetnet}
#' function using a network obtained with the \pkg{osmdata} `osmdata_sc`
#' function.
#' @param gtfs A 'GTFS' feed extracted with the \pkg{gtfsrouter} function,
#' 'extract_gtfs'.
#' @param from Vector or matrix of points **from** which route distances are to
#' be calculated. If not given, times are calculated from all points in the
#' network.
#' @param start_time_limits As for the 'gtfs_traveltimes' function of
#' \pkg{gtfsrouter}, a vector of two values specifying the earliest and latest
#' departure times from each station.
#'
#' @family main
#' @export
m4ra_times_multi_mode <- function (graph,
                                   gtfs,
                                   from = NULL,
                                   start_time_limits = NULL) {

    # Final times from GTFS stops out to network points are only calcualted from
    # this many potential stops for each start point. See the C++ code in
    # "travel_times" for details. Varying this value should not make any great
    # difference, except for slowing down algorithm for larger values.
    n_closest <- 10L

    message (cli::symbol$play,
        cli::col_green (" Calculating times to all GTFS stops "),
        appendLF = FALSE)
    utils::flush.console ()
    tmat_net_gtfs <-
        m4ra_times_to_gtfs_stops (graph, gtfs, from = from, graph_to_gtfs = TRUE)
    message ("\r", cli::col_green (cli::symbol$tick,
        " Calculated times to all GTFS stops   "))

    message (cli::symbol$play,
        cli::col_green (" Calculating times between all GTFS stops "),
        appendLF = FALSE)
    utils::flush.console ()
    tmat_gtfs_gtfs <-
        m4ra_gtfs_traveltimes (gtfs, start_time_limits = start_time_limits)
    message ("\r", cli::col_green (cli::symbol$tick,
        " Calculated times between all GTFS stops   "))

    message (cli::symbol$play,
        cli::col_green (" Calculating times back out to all network points "),
        appendLF = FALSE)
    utils::flush.console ()
    tmat_gtfs_net <-
        m4ra_times_to_gtfs_stops (graph, gtfs, graph_to_gtfs = FALSE)
    message ("\r", cli::col_green (cli::symbol$tick,
        " Calculated times back out to all network points   "))

    # The C++ function then traces for each "from" point the shortest times
    # to all gtfs stations, then selects the `n_closest` of them, and traces
    # times from those terminal stops out to all remaining network points,
    # updating any shortest times found.
    v <- dodgr::dodgr_vertices (graph)
    if (!is.null (from)) {
        v <- v [match (from, v$id), ]
    }
    closest_stns <- rcpp_closest_gtfs (v, gtfs$stops, n_closest = n_closest)

    message (cli::symbol$play,
        cli::col_green (" Combining matrices to generate final travel times "),
        appendLF = FALSE)
    utils::flush.console ()
    final_times <- rcpp_net_gtfs_travel_times (
        tmat_net_gtfs,
        tmat_gtfs_gtfs,
        tmat_gtfs_net,
        closest_stns
    )
    message ("\r", cli::col_green (cli::symbol$tick,
        " Combined matrices to generate final travel times   "))

    return (final_times)
}

#' Calculate matrix of pair-wise travel times between points using multiple
#' modes of transport.
#'
#' Alias for \link{m4ra_times_multi_mode}
#' @inherit m4ra_times_multi_mode
#' @family main
#' @export
m4ra_times_multi_modal <- function (graph,
                                   gtfs,
                                   from = NULL,
                                   start_time_limits = NULL) {

    m4ra_times_multi_mode (graph, gtfs, from = from, start_time_limits = start_time_limits)
}
