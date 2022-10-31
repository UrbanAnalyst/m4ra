
#' Prepare network and GTFS components for 'm4ra' queries.
#'
#' This is a meta-function that calls the following functions:
#' \itemize{
#' \item \link{m4ra_weight_networks} to generate several versions of the input
#' street network weighted for different kinds of transport.
#' \item \link{m4ra_gtfs_traveltimes} to generate 
#' }
#' It also identifies the closest GTFS stops to every network point. The
#' function stores several differently-weighted version of the street networks
#' in a local cache directory defined by the `user_cache_dir()` function of the
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
#' @param city_name Name of city used to name cached files.
#' @param day As for the 'gtfs_traveltimes' function of \pkg{gtfsrouter}, the
#' day for which the matrix is to be calculated.
#' @param start_time_limits As for the 'gtfs_traveltimes' function of
#' \pkg{gtfsrouter}, a vector of two values specifying the earliest and latest
#' departure times from each station.
#' @param final_mode The mode of transport used for the final stage from GTFS
#' stops to destination points.
#' @param fast Values of `TRUE` generate potentially enormous matrices which may
#' not fit in local memory. The default value of `FALSE` is generally safer, but
#' calculation may take considerably longer.
#' @param n_closest Final travel times to each destination point are calculated
#' by tracing back times to this number of closest GTFS stops. Lower values will
#' result in faster calculation times, yet with potentially inaccurate results.
#' @export
m4ra_prepare_data <- function (net_sc = NULL, gtfs = NULL, city_name = NULL,
                               day = NULL, start_time_limits = NULL,
                               final_mode = "foot", fast = FALSE,
                               n_closest = 10L) {

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

    files <- c (net_files, gtfs)

    f_closest_gtfs <- times_gtfs_to_net (files, mode = final_mode, fast = fast, n_closest = n_closest)

    return (c (files, fname_gtfs, f_closest_gtfs))
}

#' Identify closest GTFS stops to every network point.
#' @noRd
times_gtfs_to_net <- function (files, mode = "foot", fast = FALSE, n_closest = 10L) {

    checkmate::assert_character (mode, len = 1L)
    mode <- match.arg (mode, c ("foot", "bicycle"))

    f_net <- grep (mode, files, value = TRUE)
    if (length (f_net) != 1L) {
        stop ("files must contain a single file with mode [", mode, "]")
    }
    graph <- m4ra_load_cached_network (f_net)
    graph_hash <- get_hash (graph, contracted = FALSE, force = TRUE)
    graph_hash <- substring (graph_hash, 1L, 6L)
    message (cli::symbol$play,
        cli::col_green (" Contracting network graph"),
        appendLF = FALSE)
    utils::flush.console ()
    graph_c <- dodgr::dodgr_contract_graph (graph)
    message ("\r", cli::col_green (cli::symbol$tick,
        " Contracted network graph    "))

    city <- regmatches (f_net, regexpr ("m4ra\\-.*[^\\-]\\-", f_net))
    city <- gsub ("^m4ra\\-|\\-.*$", "", city)
    f_gtfs_tmat <- grep (paste0 ("m4ra-", city, "-gtfs"), files, value = TRUE, fixed = TRUE)
    # That is the pre-processed GTFS travel times matrix, so the actual feed is
    # the other 'files' entry with "gtfs":
    f_gtfs <- grep ("gtfs", files, value = TRUE)
    if (length (f_gtfs_tmat) > 0) {
        f_gtfs <- f_gtfs [which (!f_gtfs == f_gtfs_tmat)]
    }
    gtfs <- readRDS (f_gtfs)
    gtfs_hash <- substring (digest::digest (gtfs), 1, 6)

    # match GTFS stop coordinates to unique values
    n_digits <- 6L
    stops_xy <- gtfs$stops [, c ("stop_lon", "stop_lat")]
    stops_xy$stop_lon <- format (stops_xy$stop_lon, digits = 6)
    stops_xy$stop_lat <- format (stops_xy$stop_lat, digits = 6)
    index_in <- which (!duplicated (stops_xy))
    stops_xy_un <- stops_xy [index_in, ]
    xy_char <- paste0 (stops_xy$stop_lon, "-", stops_xy$stop_lat)
    xy_un_char <- paste0 (stops_xy_un$stop_lon, "-", stops_xy_un$stop_lat)
    index_out <- match (xy_char, xy_un_char)

    stops <- gtfs$stops [index_in, ]

    fname <- paste0 ("m4ra-", city, "-gtfs-to-net-", gtfs_hash, "-", graph_hash, ".Rds")
    fname <- file.path (m4ra_cache_dir (), fname)

    if (!file.exists (fname)) {
        if (fast) {
            closest_gtfs <- closest_gtfs_to_net_fast (graph_c, stops, n_closest = n_closest)
            closest_gtfs <- apply (closest_gtfs, 2, function (i)
                                   i [which (!is.na (i))], simplify = FALSE)
        } else {
            closest_gtfs <- closest_gtfs_to_net_slow (graph_c, stops, n_closest = n_closest)
        }

        closest_gtfs <- rcpp_expand_closest_index (closest_gtfs, index_out - 1L)

        saveRDS (closest_gtfs, fname)
    }

    return (fname)
}

closest_gtfs_to_net_fast <- function (graph_c, stops, n_closest) {

    v <- dodgr::dodgr_vertices (graph_c)
    ids <- v$id [dodgr::match_points_to_verts (v, stops [, c ("stop_lon", "stop_lat")])]

    dmat <- dodgr::dodgr_distances (graph_c, from = ids)

    maxd <- rcpp_matrix_max (dmat)
    dmat [is.na (dmat)] <- maxd

    return (rcpp_closest_pts (dmat, n_closest, maxd))
}

closest_gtfs_to_net_slow <- function (graph_c, stops, n_closest) {

    v <- dodgr::dodgr_vertices (graph_c)
    
    return (rcpp_closest_gtfs (v, stops, n_closest))
}
