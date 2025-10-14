#' Construct a travel time matrix between all pairs of stops in a 'GTFS' feed.
#'
#' @param gtfs A 'GTFS' feed extracted with the \pkg{gtfsrouter} function,
#' 'extract_gtfs'.
#' @param start_time_limits As for the 'gtfs_traveltimes' function of
#' \pkg{gtfsrouter}, a vector of two values specifying the earliest and latest
#' departure times from each station.
#' @param day As for the 'gtfs_traveltimes' function of \pkg{gtfsrouter}, the
#' day for which the matrix is to be calculated.
#' @param from_stops If not `NULL` (default), calculate travel times only from
#' the stops identified in this parameter. All values must be stops taken from
#' `gtfs$stops$stop_id`.
#' @param next_interval If `TRUE`, calculate time intervals to subsequent trips
#' after identified fastest trips. These subsequent trips may not necessarily be
#' as fast as the initial trips, and so this requires a second calculation of
#' all travel times. Setting this parameter to `TRUE` will therefore generally
#' double the calculation time. Note that intervals to subsequent trips may be
#' negative where alternative connections with greater numbers of transfers
#' leave earlier than initial, minimal-transfer trips.
#' @param quiet If `FALSE`, display progress information on screen.
#' @return A list of two or three integer matrices:
#' \itemize{
#' \item "duration": The fastest travel times between all pairs of stops for the
#' specified 'start_time_limits'; and
#' \item "ntransfers": The corresponding numbers of transfers.
#' \item (Only if 'next_interval = TRUE') "intervals": a matrix of intervals (in
#' seconds) until the next fastest service after that corresponding to the times
#' in the 'duration' item.
#' }
#' @family main
#' @export

m4ra_gtfs_traveltimes <- function (gtfs,
                                   start_time_limits,
                                   day,
                                   from_stops = NULL,
                                   next_interval = TRUE,
                                   quiet = FALSE) {

    if (!"timetable" %in% names (gtfs)) {
        gtfs <- gtfsrouter::gtfs_timetable (gtfs, day = day)
    }

    # gtfsrouter internal fn:
    start_time_limits <- convert_start_time_limits (start_time_limits)

    stops <- gtfs$stops$stop_id
    if (!is.null (from_stops)) {
        checkmate::assert_character (from_stops)
        checkmate::assert_true (all (from_stops %in% gtfs$stops$stop_id))
        stops <- from_stops
    }

    num_cores <- get_num_cores () # in utils.R

    # gtfsrouter::traveltimes default params:
    minimise_transfers <- FALSE
    max_traveltime <- 60 * 60

    tdir <- fs::dir_ls (fs::path_temp (), regexp = "gtfstemp")
    if (length (tdir) > 0L) {
        checkmate::assert_character (tdir, len = 1L)
        checkmate::assert_directory_exists (tdir)
    }

    # mclapply sometimes fails to deliver results; this first wrapper ensures
    # results are generated:
    res <- list (integer (0L))
    lens <- vapply (res, length, integer (1L))
    count <- 0L
    while (any (lens == 0L)) {

        res <- parallel::mclapply (stops, function (s) {

            from_is_id <- TRUE
            grep_fixed <- FALSE
            # gtfsrouter internal fn:
            start_stns <- station_name_to_ids (s, gtfs, from_is_id, grep_fixed)

            # gtfsrouter internal fn:
            stns <- rcpp_traveltimes (
                gtfs$timetable,
                gtfs$transfers,
                nrow (gtfs$stop_ids),
                start_stns,
                start_time_limits [1],
                start_time_limits [2],
                minimise_transfers,
                max_traveltime
            )

            if (length (tdir) > 0L) {
                f <- fs::path (tdir, s)
                writeLines (as.character (s), f)
            }

            return (stns [-1, ]) # 3 cols: start_time, duration, ntransfers
        }, mc.cores = num_cores)

        lens <- vapply (res, length, integer (1L))
        count <- count + 1L
        if (count > 5L) {
            stop ("Unable to generate travel times.", call. = FALSE)
        }
    }

    if (next_interval) {
        if (!quiet) {
            cli::cli_alert_info (cli::col_blue (
                "Calculating second GTFS travel time interval matrix."
            ))
        }
        intervals <- gtfs_next_intervals (gtfs, stops, res, start_time_limits)
    }

    # Convert main result to 2 square matrices of (duration, ntransfers):
    res <- lapply (2:3, function (i) {

        res_i <- lapply (res, function (j) as.vector (j [, i]))

        res_i <- do.call (rbind, res_i)
        res_i [res_i == .Machine$integer.max] <- NA_integer_

        rownames (res_i) <- stops
        colnames (res_i) <- gtfs$stops$stop_id

        # diag (res_i) <- 0L # diagonals set in "prepare-data" routines

        return (res_i)
    })

    names (res) <- c ("duration", "ntransfers")

    if (next_interval) {
        res$intervals <- intervals
    }

    return (res)
}

#' Main function to calculate intervals to next fastest connection, called from
#' `m4ra_gtfs_traveltimes()` if `next_interval = TRUE`.
#' @noRd
gtfs_next_intervals <- function (gtfs, stops, res, start_time_limits) {

    # Get initial start_times for each stop. Start times for different end-stops
    # may differ, but generally almost all connections leave on same service,
    # the start time of which is then accurately captured by the modal value.
    start_times <- vapply (res, function (i) {
        index <- which (i [, 1] > 0 & i [, 1] < (24L * 3600L))
        out <- NA_integer_
        if (length (index) > 0L) {
            vals <- sort (i [index, 1])
            if (length (vals) == 1L) {
                out <- vals + 1L
            } else if (length (vals) == 2L) {
                out <- vals [1] + 1L
            } else {
                out <- vals [floor (length (vals) / 2)] + 1L
            }
        }
        return (out)
    }, integer (1L))
    # start_times [which (is.na (start_times))] <- start_time_limits [1]


    # call next_start_times fn to get times of next services:
    next_starts <- gtfs_next_start_times (
        gtfs,
        stops,
        start_times,
        diff (start_time_limits)
    )
    next_starts [next_starts <= 0] <- NA_integer_

    first_starts <- lapply (res, function (i) as.vector (i [, 1]))
    first_starts <- do.call (rbind, first_starts)
    first_starts [first_starts == .Machine$integer.max | first_starts <= 0] <- NA_integer_
    next_interval <- next_starts - first_starts

    # diagonals set in prepare-data routines:
    # diag (next_interval) <- NA_integer_

    rownames (next_interval) <- stops
    colnames (next_interval) <- gtfs$stops$stop_id

    return (next_interval)
}

gtfs_next_start_times <- function (gtfs, stops, start_times, start_interval) {

    stop_names <- stops
    stops <- lapply (seq_along (stops), function (i) {
        c (stops [i], start_times [i] + c (0, start_interval))
    })

    # gtfsrouter::traveltimes default params:
    minimise_transfers <- FALSE
    max_traveltime <- 60 * 60

    num_cores <- get_num_cores () # in utils.R

    res <- parallel::mclapply (stops, function (s) {

        from_is_id <- TRUE
        grep_fixed <- FALSE
        # gtfsrouter internal fn:
        start_stn <- station_name_to_ids (s [1], gtfs, from_is_id, grep_fixed)

        start_time_limits <- as.integer (s [2:3])

        if (!is.na (start_time_limits [1])) {

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
        } else {
            stns <- array (NA_integer_, dim = c (nrow (gtfs$stops) + 1L, 3L))
        }

        return (stns [-1, ]) # 3 cols: start_time, duration, ntransfers
    }, mc.cores = num_cores)

    start_times <- lapply (res, function (i) as.vector (i [, 1]))

    start_times <- do.call (rbind, start_times)
    start_times [start_times == .Machine$integer.max] <- NA_integer_

    rownames (start_times) <- stop_names
    colnames (start_times) <- gtfs$stops$stop_id

    # diag (start_times) <- 0L # diagonals set in prepare-data routines:

    return (start_times)
}

#' Construct a travel time matrix to or from all stops in a 'GTFS' feed from or
#' to to all points in a street network.
#'
#' @param graph A `dodgr` network returned from the \link{weight_streetnet}
#' function using a network obtained with the \pkg{osmdata} `osmdata_sc`
#' function.
#' @param gtfs A 'GTFS' feed extracted with the \pkg{gtfsrouter} function,
#' 'extract_gtfs'.
#' @param city Name of city being analysed; used to name and extract cache
#' files.
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
#' @family prep
#' @export

m4ra_times_to_gtfs_stops <- function (graph,
                                      gtfs,
                                      city,
                                      from = NULL,
                                      graph_to_gtfs = TRUE) {

    checkmate::assert_class (graph, "dodgr_streetnet_sc")
    checkmate::assert_class (gtfs, "gtfs")
    checkmate::assert_logical (graph_to_gtfs)

    if (graph_to_gtfs) {
        tmat <- times_from_net_to_gtfs (graph, gtfs, city, from = from)
    } else {
        tmat <- times_from_gtfs_to_net (graph, gtfs, city)
    }

    # This rounds off decimal seconds:
    storage.mode (tmat) <- "integer"

    return (tmat)
}

match_stops_to_network <- function (graph, gtfs, city) {

    stops <- data.frame (
        stop_id = gtfs$stops$stop_id,
        name = gtfs$stops$stop_name,
        x = gtfs$stops$stop_lon,
        y = gtfs$stops$stop_lat
    )

    v <- m4ra_vertices (graph, city)
    stop_index <- dodgr::match_pts_to_verts (v, stops [, c ("x", "y")])
    stops$osm_id <- v$id [stop_index]

    return (stops)
}

times_from_net_to_gtfs <- function (graph, gtfs, city, from = NULL) {

    stops <- match_stops_to_network (graph, gtfs, city)

    stop_ids <- unique (stops$osm_id)

    # Reverse the network, and calculate times from stop_ids to all network
    # points.
    grcols <- dodgr_graph_cols (graph)
    fr_temp <- graph [[grcols$from]]
    graph [[grcols$from]] <- graph [[grcols$to]]
    graph [[grcols$to]] <- fr_temp

    to <- NULL
    if (!is.null (from)) {
        to <- from
    }

    tmat <- t (m4ra_times_single_mode (graph, from = stop_ids, to = to))

    index <- match (stops$osm_id, stop_ids)
    tmat <- tmat [, index, drop = FALSE]

    return (tmat)
}

times_from_gtfs_to_net <- function (graph, gtfs, city) {

    stops <- match_stops_to_network (graph, gtfs, city)

    stop_ids <- unique (stops$osm_id)

    tmat <- m4ra_times_single_mode (graph, from = stop_ids)

    index <- match (stops$osm_id, stop_ids)
    tmat <- tmat [index, , drop = FALSE]

    return (tmat)
}
