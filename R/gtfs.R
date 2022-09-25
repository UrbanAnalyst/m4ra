
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

    start_time_limits <- convert_start_time_limits (start_time_limits)

    stops <- unique (gtfs$stops$stop_id)

    nc <- Sys.getenv ("M4RA_NUM_CORES")
    if (nzchar (nc)) {
        num_cores <- as.integer (nc)
    } else {
        num_cores <- parallel::detectCores ()
    }

    tt <- parallel::mclapply (stops, function (s) {

        out <- rep (NA_integer_, length (stops))

        tt <- gtfsrouter::gtfs_traveltimes (
            gtfs,
            from = s,
            start_time_limits = start_time_limits,
            from_is_id = TRUE
        )
        if (nrow (tt) == 0L) {
            return (out)
        }

        index <- match (tt$stop_id, stops)
        out [index] <- tt$duration

        return (out)
    }, mc.cores = num_cores)

    tt <- do.call (rbind, tt)

    rownames (tt) <- colnames (tt) <- stops

    return (tt)
}
