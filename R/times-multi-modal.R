
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

    message (cli::symbol$play,
        cli::col_green (" Calculating times to all GTFS stops "),
        appendLF = FALSE)
    tmat_net_gtfs <-
        m4ra_times_to_gtfs_stops (graph, gtfs, from = from, graph_to_gtfs = TRUE)
    message ("\r", cli::col_green (cli::symbol$tick,
        " Calculated times to all GTFS stops   "))

    message (cli::symbol$play,
        cli::col_green (" Calculating times between all GTFS stops "),
        appendLF = FALSE)
    tmat_gtfs_gtfs <-
        m4ra_gtfs_traveltimes (gtfs, start_time_limits = start_time_limits)
    message ("\r", cli::col_green (cli::symbol$tick,
        " Calculated times between all GTFS stops   "))

    message (cli::symbol$play,
        cli::col_green (" Calculating times back out to all network points "),
        appendLF = FALSE)
    tmat_gtfs_net <-
        m4ra_times_to_gtfs_stops (graph, gtfs, graph_to_gtfs = FALSE)
    message ("\r", cli::col_green (cli::symbol$tick,
        " Calculated times back out to all network points   "))

    # The following 'apply' call then implements the following 4 steps:
    # 1. net -> gtfs: select `n` fastest times, effectively reducing 
    #    ["nverts", "nstops"] -> ["nverts", "n"]
    # 2. Find fastest times from `n` stops to all other stops, effectively
    #    reducing ["nstops", "nstops"] -> ["n", "nstops"]
    # 3. Combine 1 and 2 to generate fastest times between ["nverts", "stops"]
    # 4. Combine that with "gtfs" -> "net" times to generate final 
    #    ["nverts", "nverts"] fastest times.

    message (cli::symbol$play,
        cli::col_green (" Combining matrices to generate final travel times "),
        appendLF = FALSE)
    final_times <- apply (tmat_net_gtfs, 1, function (i) {

        # Initial travel times to each gtfs stop: [from vert, to stop]:
        imat <- matrix (i, nrow = length (i), ncol = length (i), byrow = FALSE)
        # Plus travel times from those stops to all others:
        tmat_net_plus_gtfs <- imat + tmat_gtfs_gtfs

        # Minimal times to each stop:
        tmat_net_plus_gtfs <- apply (tmat_net_plus_gtfs, 2, function (j) {
            if (all (is.na (j))) {
                return (c (NA, NA))
            }
            k <- which.min (j)
            c (j [k], k)
        })
        tmat_net_plus_gtfs <- t (tmat_net_plus_gtfs)
        # [, 1] = time
        # [, 2] = index into gtfs stops

        # minimal times from those stops to all other points:
        tmat_out <- tmat_gtfs_net [tmat_net_plus_gtfs [, 2], ]
        # That is then [nstops, nverts]; reduce to nverts:
        tvec_out <- apply (tmat_out, 2, function (j) {
            if (all (is.na (j))) {
                return (NA)
            }
            min (j, na.rm = TRUE)
        })

        return (tvec_out)
    }, simplify = TRUE)

    final_times <- t (final_times)
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
