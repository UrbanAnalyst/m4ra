
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

    return (dur)
}
