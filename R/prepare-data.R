
#' Prepare network and GTFS components for 'm4ra' queries.
#'
#' This is a meta-function that calls the following functions:
#' \itemize{
#' \item \link{m4ra_weight_networks} to generate several versions of the input
#' street network weighted for different kinds of transport.
#' \item \link{m4ra_gtfs_traveltimes} to generate 
#' }
#' This stores several differently-weighted version of the street networks in a
#' local cache directory defined by the `user_cache_dir()` function of the
#' \pkg{rappdirs} package, or by a local environment parameter,
#' "M4RA_CACHE_DIR", if set.
#'
#' @param net_sc Local file path to a \pkg{silicate}, "SC", format object
#' containing network data used to generate weighted street networks.
#' @param gtfs Local file path to a \pkg{gtfsrouter} object saved via `saveRDS`,
#' containing GTFS (General Transit Feed Specification) data for network
#' represented in `net_sc`. This `.Rds` object may include additional timetable
#' or transfer information in addition to data represented in original
#' `zip`-format data of provided GTFS feed.
#' @param day As for the 'gtfs_traveltimes' function of \pkg{gtfsrouter}, the
#' day for which the matrix is to be calculated.
#' @param start_time_limits As for the 'gtfs_traveltimes' function of
#' \pkg{gtfsrouter}, a vector of two values specifying the earliest and latest
#' departure times from each station.
#' @param city_name Name of city used to name cached files.
#' @export
m4ra_prepare_data <- function (net_sc = NULL, gtfs = NULL, start_time_limits = NULL,
                                   day = NULL, city_name = NULL) {

    cache_dir <- m4ra_cache_dir ()

    checkmate::assert_character (net_sc, max.len = 1L)
    checkmate::assert_character (gtfs, max.len = 1L)
    checkmate::assert_character (city_name, max.len = 1L)
    checkmate::assert_character (day, max.len = 1L)
    checkmate::assert_file_exists (net_sc)
    checkmate::assert_file_exists (gtfs)

    # gtfsrouter internal fn:
    start_time_limits <- convert_start_time_limits (start_time_limits)

    days <- c ("mo", "tu", "we", "th", "fr", "sa", "so")
    day <- match.arg (substring (tolower (day), 1, 2), days)
    city_name <- gsub ("\\s+", "-", tolower (city_name))

    net <- readRDS (net_sc)
    net_files <- m4ra_weight_networks (net, city = city_name, quiet = FALSE)

    gtfs_data <- readRDS (gtfs)
    gtfs_hash <- substring (digest::digest (gtfs_data), 1, 6)
    fname_gtfs <- paste0 ("m4ra-", city_name, "-gtfs-", gtfs_hash,
        "-", day, "-", paste0 (start_time_limits, collapse = "-"), ".Rds")
    fname_gtfs <- file.path (cache_dir, fname_gtfs)

    if (!file.exists (fname_gtfs)) {
        gtfs_data <- gtfsrouter::gtfs_timetable (gtfs_data, day = day)

        tmat_gtfs_gtfs <- m4ra_gtfs_traveltimes (gtfs_data, start_time_limits = start_time_limits)
        attr (tmat_gtfs_gtfs, "day") <- day
        attr (tmat_gtfs_gtfs, "start_time_limits") <- start_time_limits
        saveRDS (tmat_gtfs_gtfs, fname_gtfs)
    }

    return (c (net_files, gtfs, fname_gtfs))
}
