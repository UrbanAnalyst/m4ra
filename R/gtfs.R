
#' Construct a travel time matrix between all pairs of stops in a 'GTFS' feed.
#'
#' @param gtfs A 'GTFS' feed extracted with the \pkg{gtfsrouter} function,
#' 'extract_gtfs'.
#' @param start_time_limits As for the 'gtfs_traveltimes' function of
#' \pkg{gtfsrouter}, a vector of two values specifying the earliest and latest
#' departure times from each station.
#' @param day As for the 'gtfs_traveltimes' function of \pkg{gtfsrouter}, the
#' day for which the matrix is to be calculated.
#' @return An integer matrix of fastest travel times between all stops for the
#' specified 'start_time_limits'
#' @family main
#' @export

m4ra_gtfs_traveltimes <- function (gtfs, start_time_limits, day) {

    if (!"timetable" %in% names (gtfs)) {
        gtfs <- gtfsrouter::gtfs_timetable (gtfs)
    }

    # gtfsrouter internal fn:
    start_time_limits <- convert_start_time_limits (start_time_limits)

    stops <- gtfs$stops$stop_id

    nc <- Sys.getenv ("M4RA_NUM_CORES")
    if (nzchar (nc)) {
        num_cores <- as.integer (nc)
    } else {
        num_cores <- parallel::detectCores ()
    }

    # gtfsrouter::traveltimes default params:
    minimise_transfers <- FALSE
    max_traveltime <- 60 * 60

    dur <- parallel::mclapply (stops, function (s) {

        from_is_id <- TRUE
        grep_fixed <- FALSE
        # gtfsrouter internal fn:
        start_stn <- station_name_to_ids (s, gtfs, from_is_id, grep_fixed)

        # gtfsrouter internal fn:
        stns <- rcpp_traveltimes (
            gtfs$timetable,
            gtfs$transfers,
            nrow (gtfs$stop_ids),
            start_stn,
            start_time_limits [1],
            start_time_limits [2],
            minimise_transfers,
            max_traveltime
        )

        return (stns [-1, 2])
    }, mc.cores = num_cores)

    dur <- do.call (rbind, dur)
    dur [dur == .Machine$integer.max] <- NA_integer_

    rownames (dur) <- colnames (dur) <- stops

    diag (dur) <- 0L

    return (dur)
}

#' Construct a travel time matrix to or from all stops in a 'GTFS' feed from or
#' to to all points in a street network.
#'
#' @param graph A `dodgr` network returned from the \link{weight_streetnet}
#' function using a network obtained with the \pkg{osmdata} `osmdata_sc`
#' function.
#' @param gtfs A 'GTFS' feed extracted with the \pkg{gtfsrouter} function,
#' 'extract_gtfs'.
#' @param from Vector or matrix of points **from** which route distances are to
#' be calculated. If not given, times are calculated from all points in the
#' network. Only has any effect is `graph_to_gtfs` is `TRUE`.
#' @param graph_to_gtfs If `TRUE`, generate matrix of times from all network
#' junctions in 'graph' (or all `from` points if specified) to each stop in the
#' 'gtfs$stops' table; otherwise generate matrix of times from all stops to all
#' network junctions.
#' @return An integer matrix of fastest travel times either between all 'gtfs'
#' stops and all network points (for 'graph_to_gtfs = FALSE'), or the other way
#' around (for 'graph_to_gtfs = TRUE').
#' @family main
#' @export

m4ra_times_to_gtfs_stops <- function (graph, gtfs, from = NULL, graph_to_gtfs = TRUE) {

    checkmate::assert_class (graph, "dodgr_streetnet_sc")
    checkmate::assert_class (gtfs, "gtfs")
    checkmate::assert_logical (graph_to_gtfs)

    if (graph_to_gtfs) {
        tmat <- times_from_net_to_gtfs (graph, gtfs, from = from)
    } else {
        tmat <- times_from_gtfs_to_net (graph, gtfs)
    }

    return (tmat)
}

match_stops_to_network <- function (graph, gtfs) {

    stops <- data.frame (
        stop_id = gtfs$stops$stop_id,
        name = gtfs$stops$stop_name,
        x = gtfs$stops$stop_lon,
        y = gtfs$stops$stop_lat
    )

    v <- dodgr::dodgr_vertices (graph)
    stop_index <- dodgr::match_pts_to_verts (v, stops [, c ("x", "y")])
    stops$osm_id <- v$id [stop_index]

    return (stops)
}

times_from_net_to_gtfs <- function (graph, gtfs, from = NULL) {

    stops <- match_stops_to_network (graph, gtfs)

    stop_ids <- unique (stops$osm_id)

    # Reverse the network, and calculate times from stop_ids to all network
    # points.
    grcols <- dodgr_graph_cols (graph)
    fr_temp <- graph [[grcols$from]]
    graph [[grcols$from]] <- graph [[grcols$to]]
    graph [[grcols$to]] <- fr_temp

    to <- NULL
    if (!is.null (from)) {
        to = from
    }

    tmat <- t (m4ra_times_single_mode (graph, from = stop_ids, to = to))

    index <- match (stops$osm_id, stop_ids)
    tmat <- tmat [, index]

    return (tmat)
}

times_from_gtfs_to_net <- function (graph, gtfs) {

    stops <- match_stops_to_network (graph, gtfs)

    stop_ids <- unique (stops$osm_id)

    tmat <- m4ra_times_single_mode (graph, from = stop_ids)

    index <- match (stops$osm_id, stop_ids)
    tmat <- tmat [index, ]

    return (tmat)
}
