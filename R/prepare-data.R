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
#' @param planet_file Optional file path to local `.osm.pbf` or `.osm.bz2` file
#' encompassing specified bounding box. If given, data are extracted with
#' system-level calls to "osmium", which must be installed.
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
#' @param parking If `TRUE`, calculate local densities of parking availability
#' and building volumes, and convert to a score used to calculate time penalties
#' for automobile routing.
#' @param quiet If `FALSE`, display progress information on screen.
#' @family prep
#' @export
m4ra_prepare_data <- function (net_sc = NULL,
                               gtfs = NULL,
                               city_name = NULL,
                               planet_file = NULL,
                               day = NULL,
                               start_time_limits = NULL,
                               final_mode = "foot",
                               fast = FALSE,
                               n_closest = 10L,
                               parking = FALSE,
                               quiet = FALSE) {

    pt0_whole <- proc.time ()

    if (!is.null (net_sc)) {
        checkmate::assert_character (net_sc, max.len = 1L)
        checkmate::assert_file_exists (net_sc)
    }
    if (!is.null (gtfs)) {
        checkmate::assert_character (gtfs, max.len = 1L)
        checkmate::assert_file_exists (gtfs)
    }
    checkmate::assert_character (city_name, max.len = 1L)
    if (!is.null (day)) {
        checkmate::assert_character (day, max.len = 1L)
        days <- c ("mo", "tu", "we", "th", "fr", "sa", "so")
        day <- match.arg (substring (tolower (day), 1, 2), days)
    }
    if (!is.null (start_time_limits)) {
        # gtfsrouter internal fn:
        start_time_limits <- convert_start_time_limits (start_time_limits)
    }

    city_name <- gsub ("\\s+", "-", tolower (city_name))

    cache_dir <- fs::path (m4ra_cache_dir (), city_name)

    if (!is.null (net_sc)) {
        # (Re-)generate weighted networks
        net <- m_readRDS (net_sc)
        net_files <- m4ra_weight_networks (net, city = city_name, quiet = quiet)
    } else {
        # Just read weighted network files from cache_dir contents
        ptn <- paste0 (
            "m4ra\\-",
            city_name,
            "\\-(bicycle|foot|motorcar|vert\\-index)\\-"
        )
        net_files <- fs::dir_ls (cache_dir, regexp = ptn)
    }

    if (!is.null (gtfs)) {

        # (Re-)generate GTFS travel time matrix
        fnames_gtfs <- times_gtfs_to_gtfs (
            gtfs,
            city_name,
            cache_dir,
            day,
            start_time_limits,
            quiet
        )

    } else {

        ptn <- paste0 (city_name, "\\-gtfs\\-.*[0-9]{4,5}\\-[0-9]{4,5}\\.Rds$")
        fnames_gtfs <- fs::dir_ls (cache_dir, regexp = ptn)
        if (length (fnames_gtfs) > 1L) {
            if (is.null (start_time_limits)) {
                stop (
                    "Multiple pre-processed GTFS timetables found; ",
                    "please specify 'start_time_limits'",
                    call. = FALSE
                )
            }
            start_time_limits <- convert_start_time_limits (start_time_limits)
            fnames_gtfs <- grep (
                paste0 (start_time_limits, collapse = "-"),
                fnames_gtfs,
                value = TRUE
            )
            fnames_gtfs <- grep (
                paste0 ("\\-", day, "\\-"),
                fnames_gtfs,
                value = TRUE
            )
            fnames_gtfs <- grep (
                "\\-(times|transfers|intervals)\\-",
                fnames_gtfs,
                value = TRUE
            )
            if (length (fnames_gtfs) > 3L) {
                warning (
                    "Multiple pre-processed GTFS timetables found ",
                    "for specified 'start_time_limits' and 'day'; ",
                    "the first will be selected."
                )
                fnames_gtfs <- fnames_gtfs [1:3]
            }
        }
    }

    files <- c (net_files, gtfs, fnames_gtfs)

    f_closest_gtfs <- times_gtfs_to_net (
        files,
        mode = final_mode,
        fast = fast,
        n_closest = n_closest,
        quiet = quiet
    )
    files <- unique (c (files, f_closest_gtfs))

    f_parking <- NULL
    if (parking) {

        if (is.null (net_sc)) {
            stop (
                "Parking requires specification of 'net_sc' parameter",
                call. = FALSE
            )
        }

        dat_p <- m4ra_parking (
            city_name = city_name,
            mode = "motorcar",
            planet_file = planet_file,
            dlim = 5000,
            k = 1000,
            quiet = quiet
        )
        f <- fs::dir_ls (cache_dir, regexp = city_name, fixed = TRUE)
        f_parking <- grep ("parking", f, value = TRUE)
    }

    if (!quiet) {
        cli::cli_alert_success (cli::col_green (
            "Total time for data preparation: ", process_time (pt0_whole)
        ))
    }

    return (c (files, f_parking))
}

#' Generate GTFS transport time matrix bewtween all station pairs
#' @noRd
times_gtfs_to_gtfs <- function (gtfs,
                                city_name,
                                cache_dir,
                                day,
                                start_time_limits,
                                quiet) {

    gtfs_data <- m_readRDS (gtfs)
    gtfs_hash <- substring (digest::digest (gtfs_data), 1, 6)
    fname_gtfs_times <- paste0 (
        "m4ra-", city_name, "-gtfs-", gtfs_hash,
        "-times-", substring (tolower (day), 1, 2),
        "-", paste0 (start_time_limits, collapse = "-"), ".Rds"
    )
    fname_gtfs_times <- fs::path (cache_dir, fname_gtfs_times)

    if (!fs::file_exists (fname_gtfs_times)) {

        ptn <- paste0 (
            "m4ra\\-",
            city_name,
            "\\-gtfs\\-.*[0-9]{4,5}\\-[0-9]{4,5}\\.Rds"
        )
        fname_gtfs_pre <- fs::dir_ls (cache_dir, regexp = ptn)
        if (!is.null (start_time_limits)) {
            ptn <- paste0 ("\\-", start_time_limits [1], "\\-", start_time_limits [2], "\\.Rds$")
            fname_gtfs_pre <- grep (ptn, fname_gtfs_pre, value = TRUE)
        }
        if (!is.null (day)) {
            ptn <- paste0 ("\\-", day, "\\-")
            fname_gtfs_pre <- grep (ptn, fname_gtfs_pre, value = TRUE)
        }
        if (length (fname_gtfs_pre) > 0) {
            fname_gtfs_times <- grep ("\\-times\\-", fname_gtfs_pre, value = TRUE)
        }
    }
    fname_gtfs_transfers <- gsub ("\\-times\\-", "-transfers-", fname_gtfs_times)
    fname_gtfs_intervals <- gsub ("\\-times\\-", "-intervals-", fname_gtfs_times)

    if (!fs::file_exists (fname_gtfs_times)) {

        if (!quiet) {
            pt0 <- proc.time ()
            n_stns <- format (nrow (gtfs_data$stops), big.mark = ",")
            cli::cli_alert_info (cli::col_blue (
                "Calculating GTFS travel time matrix between ",
                n_stns, " stops."
            ))
        }
        gtfs_data <- gtfsrouter::gtfs_timetable (gtfs_data, day = day)

        res_gtfs_gtfs <- m4ra_gtfs_traveltimes (
            gtfs_data,
            start_time_limits = start_time_limits,
            next_interval = TRUE,
            quiet = quiet
        )

        # travel times:
        attr (res_gtfs_gtfs$duration, "day") <- day
        attr (res_gtfs_gtfs$duration, "start_time_limits") <- start_time_limits
        saveRDS (res_gtfs_gtfs$duration, fname_gtfs_times)
        res_gtfs_gtfs$duration <- NULL

        # transfers:
        attr (res_gtfs_gtfs$ntransfers, "day") <- day
        attr (res_gtfs_gtfs$ntransfers, "start_time_limits") <- start_time_limits
        saveRDS (res_gtfs_gtfs$ntransfers, fname_gtfs_transfers)
        res_gtfs_gtfs$ntransfers <- NULL

        # intervals
        attr (res_gtfs_gtfs$intervals, "day") <- day
        attr (res_gtfs_gtfs$intervals, "start_time_limits") <- start_time_limits
        saveRDS (res_gtfs_gtfs$intervals, fname_gtfs_intervals)
        rm (res_gtfs_gtfs)

        if (!quiet) {
            cli::cli_alert_success (cli::col_green (
                "Calculated GTFS travel time matrices in ", process_time (pt0)
            ))
        }
    }

    return (unname (c (
        fname_gtfs_times,
        fname_gtfs_transfers,
        fname_gtfs_intervals
    )))
}

#' Identify closest GTFS stops to every network point.
#' @noRd
times_gtfs_to_net <- function (files,
                               mode = "foot",
                               fast = FALSE,
                               n_closest = 10L,
                               quiet = FALSE) {

    checkmate::assert_character (mode, len = 1L)
    mode <- match.arg (mode, c ("foot", "bicycle"))

    cache_files <- grep (m4ra_cache_dir (), files, fixed = TRUE, value = TRUE)
    dirs <- fs::path_split (cache_files) [[1]]
    city <- dirs [length (dirs) - 1L]

    f_gtfs_to_net <- grep (
        paste0 (city, "\\-gtfs\\-to\\-net\\-"),
        fs::dir_ls (fs::path (m4ra_cache_dir (), city)),
        value = TRUE
    )
    f_gtfs_to_net <- grep (
        paste0 ("\\-", mode, "\\-"),
        f_gtfs_to_net,
        value = TRUE
    )
    f_gtfs_to_net <- grep (
        paste0 ("\\-", ifelse (fast, "fast", "slow"), "\\.Rds"),
        f_gtfs_to_net,
        value = TRUE
    )

    if (length (f_gtfs_to_net) == 1L) {
        return (f_gtfs_to_net)
    }
    # Otherwise extract hashes to check whether times need to be re-calculated:

    graph <- m4ra_load_cached_network (
        city = city,
        mode = mode,
        contracted = FALSE
    )
    graph_hash <- substring (attr (graph, "hash"), 1L, 6L)

    f_gtfs_tmat <- grep (
        paste0 ("m4ra-", city, "-gtfs.*[0-9]+\\-[0-9]+\\.Rds$"),
        files,
        value = TRUE,
        fixed = FALSE
    )
    # That is the pre-processed GTFS travel times matrix, so the actual feed is
    # the other 'files' entry with "gtfs":
    f_gtfs <- grep ("gtfs", files, value = TRUE)
    if (length (f_gtfs_tmat) > 0) {
        f_gtfs <- f_gtfs [which (!f_gtfs %in% f_gtfs_tmat)]
    }
    if (length (f_gtfs) > 1L) {
        f_gtfs <- f_gtfs [which (!grepl ("\\-to\\-net\\-", f_gtfs))]
    }
    checkmate::assert_character (f_gtfs, max.len = 1L)
    gtfs <- m_readRDS (f_gtfs)
    stops <- gtfs$stops
    # Generate hash only from the stops table, so timetable can be updated, but
    # the final stage will only need to be re-calculated if the stops themselves
    # actually change. Note also that the stops table may have duplicated
    # coordinates, and that several stops may also map onto the same network
    # vertices.
    gtfs_hash <- substring (digest::digest (stops), 1, 6)

    fname <- paste0 (
        "m4ra-",
        city,
        "-gtfs-to-net-",
        mode,
        "-ncl",
        n_closest,
        "-",
        gtfs_hash,
        "-",
        graph_hash,
        "-",
        ifelse (fast, "fast", "slow"),
        ".Rds"
    )
    fname <- fs::path (m4ra_cache_dir (), city, fname)

    if (!fs::file_exists (fname)) {

        if (!quiet) {
            cli::cli_alert_info (cli::col_blue ("Contracting network graph"))
        }
        graph_c <- m4ra_load_cached_network (
            city = city,
            mode = mode,
            contracted = TRUE
        )
        if (!quiet) {
            cli::cli_alert_success (cli::col_green ("Contracted network graph"))
        }

        v <- m4ra_vertices (graph_c, city)
        n_closest <- update_n_closest (v, stops, n_closest, quiet = quiet)

        # NOTE that all closest_gtfs indices are 0-based for direct submission
        # to C++ routines

        if (fast) {

            if (!quiet) {
                pt0 <- proc.time ()
                cli::cli_alert_info (cli::col_blue (
                    "Calculating times from terminal GTFS stops"
                ))
            }
            pt0 <- proc.time ()

            # This returns a matrix of combined distances and indices into the
            # original GTFS stops table
            closest_gtfs <- closest_gtfs_to_net_fast (
                graph_c,
                stops,
                city,
                n_closest = n_closest
            )

            closest_gtfs [is.na (closest_gtfs)] <- -1

            if (!quiet) {
                cli::cli_alert_success (cli::col_green (
                    "Calculated times from terminal GTFS stops in ",
                    process_time (pt0)
                ))
                pt0 <- proc.time ()
                cli::cli_alert_info (cli::col_blue (
                    "Converting times to indices at each GTFS stop"
                ))
            }

            # This rcpp routine converts the [n_closest, nverts] array into a
            # list of indexes and distances, once for each GTFS station. Indices
            # are then back into verts.
            closest_gtfs <- rcpp_expand_closest_index (closest_gtfs)

            if (!quiet) {
                cli::cli_alert_success (cli::col_green (
                    "Converted times to indices at each GTFS stop in ",
                    process_time (pt0)
                ))
            }
        } else {

            # This routine directly returns the expanded list into all original
            # GTFS stops:
            closest_gtfs <- closest_gtfs_to_net_slow (
                graph_c,
                stops,
                city,
                n_closest = n_closest,
                quiet = quiet
            )
        }

        n <- length (closest_gtfs) / 2
        closest <- list (
            index = closest_gtfs [seq_len (n)],
            d = closest_gtfs [n + seq_len (n)]
        )

        saveRDS (closest, fname)
    }

    return (fname)
}

#' Multiple GTFS stops can have same coordinates, and/or map on to same network
#' vertices. The mapping of vertices onto stations is based on closest stations,
#' but if there are more stations with equivalent minimal distances than the
#' value of `n_closest`, then the actual stations chosen are unpredictable, and
#' will likely result in some network vertices not having any associated
#' stations. The solution is to ensure that the value of `n_closest` is greater
#' than the maximal number of stations which map onto the same network vertex.
#'
#' @param v Network vertices from `m4ra_vertices()`
#' @noRd
update_n_closest <- function (v, stops, n_closest, quiet = FALSE) {

    pts <- v$id [dodgr::match_points_to_verts (
        v, stops [, c ("stop_lon", "stop_lat")]
    )]

    n_closest_min <- max (table (pts)) + 1L
    if (n_closest_min > n_closest) {
        if (!quiet) {
            cli::cli_alert_info (cli::col_blue (
                "Updating 'n_closest' to ", n_closest_min
            ))
        }
        n_closest <- n_closest_min
    }

    return (n_closest)
}

closest_gtfs_to_net_fast <- function (graph_c, stops, city, n_closest) {

    v <- m4ra_vertices (graph_c, city)
    ids <- v$id [dodgr::match_points_to_verts (
        v, stops [, c ("stop_lon", "stop_lat")]
    )]

    # ids can have duplicates through distinct stops mapping onto single points
    index_in <- which (!duplicated (ids))
    index_out <- match (ids, ids [index_in])
    ids <- ids [index_in]

    tmat <- m4ra_times_single_mode (graph_c, from = ids)
    # Identify points which effectively can not be reached, and set times to NA:
    nna <- apply (tmat, 2, function (i) length (which (!is.na (i))))
    tmat [, which (nna < (nrow (tmat) / 2))] <- NA

    maxt <- rcpp_matrix_max (tmat)
    tmat [is.na (tmat)] <- maxt

    # re-expand out to full stops:
    tmat <- tmat [index_out, ]

    rcpp_closest_pts (tmat, n_closest, maxt)
}

closest_gtfs_to_net_slow <- function (graph_c, stops, city,
                                      n_closest, quiet = FALSE) {

    v <- m4ra_vertices (graph_c, city)
    from <- v$id
    to <- v$id [dodgr::match_points_to_verts (
        v, stops [, c ("stop_lon", "stop_lat")]
    )]

    # to can have duplicates through distinct stops mapping onto single points
    index_in <- which (!duplicated (to))
    index_out <- match (to, to [index_in])
    to <- to [index_in]

    # That `dodgr` internal function returns zero-based indices.
    to_from_indices <- to_from_index_with_tp (graph_c, from, to)

    if (!quiet) {
        pt0 <- proc.time ()
        cli::cli_alert_info (cli::col_blue (
            "Calculating times to all terminal GTFS stops"
        ))
    }
    dmat <- rcpp_dists_to_n_targets (
        graph_c,
        to_from_indices$vert_map,
        to_from_indices$from$index,
        to_from_indices$to$index,
        n_closest
    )
    if (!quiet) {
        cli::cli_alert_success (cli::col_green (
            "Calculated times to all terminal GTFS stops in ",
            process_time (pt0)
        ))
    }

    # The 2nd half of dmat (dmat [, 11:20] for n_closest = 10, say) then holds
    # indices into the vertices of the network. These are the same as the values
    # passed from `to_from_indices`, which are indices into the network
    # vertices. These need to be re-mapped on to indices into the reduced GTFS
    # stops:
    to_index <- to_from_indices$to$index
    index <- seq_len (n_closest) + n_closest
    imat <- dmat [, index]
    storage.mode (imat) <- "integer"
    # match - 1 to convert to 0-based C++ indices:
    imat <- array (match (imat, to_index) - 1L, dim = dim (imat))
    dmat [, index] <- imat

    dmat [is.na (dmat)] <- -1

    # Then convert those index values into lists of all original stops which
    # map onto to the indicated stops. This routine also re-maps the vectors of
    # distances out to full expanded distances
    if (!quiet) {
        pt0 <- proc.time ()
        cli::cli_alert_info (cli::col_blue (
            "Converting times to indices at each GTFS stop"
        ))
    }
    res <- rcpp_remap_verts_to_stops (dmat, index_out - 1L)
    if (!quiet) {
        cli::cli_alert_success (cli::col_green (
            "Converted times to indices at each GTFS stop in ",
            process_time (pt0)
        ))
    }

    return (res)
}
