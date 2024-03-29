#' Calculate matrix of pair-wise travel times between points using multiple
#' modes of transport.
#'
#' @param initial_mode Initial mode of transport from origin points towards
#' public transport stop.
#' @param from List of OSM vertex IDs from which to calculate total multi-modal
#' travel times. These must be vertices from the largest connected component of
#' the contracted graph.
#' @param duration_max If specified, only calculate times from each point to the
#' nearest GTFS stops out to specified maximal duration in seconds. Values may
#' be provided if overall travel times are only of interest out to some defined
#' range or maximal value. Specifying values for this parameter can can
#' considerably reduce calculation times.
#' @inheritParams m4ra_prepare_data
#' @family main
#' @export
m4ra_times_multi_mode <- function (net_sc = NULL,
                                   gtfs = NULL,
                                   city_name = NULL,
                                   day = NULL,
                                   start_time_limits = NULL,
                                   initial_mode = "foot",
                                   final_mode = "foot",
                                   from = NULL,
                                   duration_max = NULL,
                                   fast = FALSE,
                                   n_closest = 10L,
                                   quiet = FALSE) {

    if (!is.null (duration_max)) {
        checkmate::assert_numeric (duration_max, len = 1L)
    }

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

    graph_c <- m4ra_load_cached_network (
        city = city_name,
        mode = initial_mode,
        contracted = TRUE
    )
    v <- m4ra_vertices (graph_c, city_name)
    if (!all (from %in% v$id)) {
        stop (
            "Not all specified 'from' vertices are part of the ",
            "largest connected component of the street network."
        )
    }

    # need times to all vertices to extract overall minimal time from initial
    # mode vs GTFS-routed modes at end. The times here are then reduced to times
    # to the GTFS stops by matching vertices afterward.
    if (!quiet) {
        cli::cli_alert_info (cli::col_blue (
            "Calculating single-mode travel times for {initial_mode}"
        ))
    }
    times <- m4ra_times_single_mode (graph_c, from = from)
    # Convert to integer storage (#16):
    times <- round (times)
    storage.mode (times) <- "integer"
    times [is.na (times)] <- -1L

    stops <- m_readRDS (gtfs)$stops
    to <- v$id [dodgr::match_points_to_verts (
        v, stops [, c ("stop_lon", "stop_lat")]
    )]
    times_to_gtfs <- times [, match (to, colnames (times)), drop = FALSE]
    if (!quiet) {
        cli::cli_alert_success (cli::col_blue (
            "Calculated single-mode travel times for {initial_mode}"
        ))
    }

    if (is.null (duration_max)) {
        stops_index <- seq_len (ncol (times_to_gtfs))
    } else {
        # Trace GTFS trips only from stops reachable within maximal time limit.
        # This is done here to immediately reduce size of matrices after
        # loading, because it can have huge memory effects of cities like Paris
        # with 30,000 steps.
        stops_index <- apply (
            times_to_gtfs, 1,
            function (i) which (i <= duration_max)
        )
        stops_index <- sort (unique (unlist (stops_index)))
    }
    times_to_gtfs <- times_to_gtfs [, stops_index]

    f <- grep ("gtfs\\-.*[0-9]{5}\\-[0-9]{5}\\.Rds$", files, value = TRUE)

    f_times <- grep ("\\-times\\-", f, value = TRUE)
    gtfs_times_mat <- m_readRDS (f_times)
    gtfs_times_mat <- gtfs_times_mat [stops_index, ]
    gtfs_times_mat [is.na (gtfs_times_mat)] <- -1L

    f_transfers <- grep ("\\-transfers\\-", f, value = TRUE)
    gtfs_transfers_mat <- m_readRDS (f_transfers)
    gtfs_transfers_mat <- gtfs_transfers_mat [stops_index, ]
    gtfs_transfers_mat [is.na (gtfs_transfers_mat)] <- -1L

    f_intervals <- grep ("\\-intervals\\-", f, value = TRUE)
    gtfs_intervals_mat <- m_readRDS (f_intervals)
    gtfs_intervals_mat <- gtfs_intervals_mat [stops_index, ]
    gtfs_intervals_mat [is.na (gtfs_intervals_mat)] <- -1L

    f <- grep ("gtfs\\-to\\-net", files, value = TRUE)
    f <- grep (paste0 ("\\-", final_mode, "\\-"), f, value = TRUE)
    gtfs_to_net <- m_readRDS (f)

    # Then convert initial times to nearest GTFS stops to times through entire
    # GTFS network to all termimal network vertices.
    if (initial_mode == final_mode) {

        v_c_final_mode <- v
        nverts_out <- nrow (v)
        ids_out <- v$id

    } else {

        graph_c_final_mode <- m4ra_load_cached_network (
            city = city_name,
            mode = final_mode,
            contracted = TRUE
        )
        v_c_final_mode <- m4ra_vertices (graph_c_final_mode, city_name)
        nverts_out <- nrow (v_c_final_mode)
        ids_out <- v_c_final_mode$id
    }

    if (!quiet) {
        cli::cli_alert_info (cli::col_blue (
            "Calculating multi-mode travel times"
        ))
    }
    # This calculates strict multi-modal times only, some of which may be slower
    # than single-mode times calculated in "times" matrix.
    res <- rcpp_add_net_to_gtfs (
        times_to_gtfs,
        cbind (gtfs_times_mat, gtfs_transfers_mat, gtfs_intervals_mat),
        gtfs_to_net$index,
        gtfs_to_net$d,
        nverts_out
    )
    rm (times_to_gtfs, gtfs_times_mat, gtfs_transfers_mat, gtfs_intervals_mat)

    if (!quiet) {
        cli::cli_alert_success (cli::col_blue (
            "Calculated multi-mode travel times"
        ))
    }

    # res is then 3 matrices of (times, transfers, intervals)
    index <- seq_len (nverts_out)
    res_times <- res [, index]
    res_transfers <- res [, index + nverts_out]
    res_intervals <- res [, index + 2 * nverts_out]

    maxr <- rcpp_matrix_max (res_times)
    res_times [res_times == maxr] <- NA_integer_
    res_transfers [res_transfers == maxr] <- NA_integer_
    res_intervals [res_intervals == maxr] <- NA_integer_

    # Replace multi-modal times with direct initial_mode times where they are
    # faster:
    index <- which (times < res_times)
    res_times [index] <- times [index]
    res_transfers [index] <- NA_integer_
    res_intervals [index] <- NA_integer_

    rownames (res_times) <- from
    colnames (res_times) <- ids_out
    rownames (res_transfers) <- from
    colnames (res_transfers) <- ids_out
    rownames (res_intervals) <- from
    colnames (res_intervals) <- ids_out

    # At that stage, `res_times` holds the fastest values routed through the
    # GTFS network. They just then have to be compared with the
    # previously-calculated times to all vertices using "initial_mode".

    if (initial_mode != final_mode) {

        times <- remap_times_to_final_network (
            res_times,
            times,
            v_c_final_mode,
            v
        )
    }

    res_times <- rcpp_min_from_two_matrices (res_times, times)

    # Then need once again to re-instate row and col names:
    rownames (res_times) <- from
    colnames (res_times) <- ids_out

    # And then finally, not all points may be reachable by "final mode"; any
    # that aren't may be re-mapped to single-mode times with "initial mode".
    # These need to be caught and removed. 'gtfs_to_net$index' values are
    # 0-indexed!
    final_mode_index <- sort (unique (unlist (gtfs_to_net$index))) + 1
    rm (gtfs_to_net)

    res_times <- res_times [, final_mode_index, drop = FALSE]
    res_transfers <- res_transfers [, final_mode_index, drop = FALSE]
    res_intervals <- res_intervals [, final_mode_index, drop = FALSE]

    return (list (
        times = res_times,
        transfers = res_transfers,
        intervals = res_intervals
    ))
}

#' Remap single-model travel times with specified initial mode on to vertices of
#' 'final_mode' network.
#'
#' @param times Initial matrix of travel times from the specified 'from' points
#' to all points on the network corresponding to the initial travel mode.
#' @param res Final matrix of multi-modal travel times to all terminal vertices
#' of the network corresponding to the final travel mode.
#' @param v_times The `m4ra_vertices` of the networks corresponding to the
#' initial travel mode.
#' @param v_res The `m4ra_vertices` of the networks corresponding to the final
#' travel mode.
#' @param A re-shaped version of 'times' with the same dimensions as 'res', and
#' times mapped on to nearest vertices in "final mode" network where required.
#'
#' @noRd
remap_times_to_final_network <- function (res, times, v_res, v_times) {

    index <- match (colnames (res), colnames (times))
    times_out <- times [, index]

    # Vertices in 'res' that have are not in 'times':
    index_no_match <- which (is.na (index))
    no_match <- colnames (res) [index_no_match]
    v_no_match <- v_res [match (no_match, v_res$id), ]
    # Match those to nearest points in "initial mode" network:
    index <- dodgr::match_points_to_verts (v_times, v_no_match [, c ("x", "y")])
    times_out [, index_no_match] <- times [, index]
    colnames (times_out) <- colnames (res)

    return (times_out)
}

#' Calculate relative times from a specified point by multi-modal transport
#' versus private automobile, along with (optionally) corresponding walking
#' distances.
#'
#' This city expects weighted networks to have been generated with the
#' \link{m4ra_batch_weight_networks} function, and for corresponding networks to
#' exist in the `m4ra` cache directory for the specified city.
#'
#' @param from Vector of OSM ID values of vertices from which ratios are to be
#' calculated. Typically obtained by loading one weighted network, and
#' sampling or extracting vertices from the function
#' `m4ra_vertices`.
#' @param walk_dists If `TRUE`, also calculate equivalent walking distances.
#' @inheritParams m4ra_times_multi_mode
#' @return A `data.frame` of destination vertices, including Open Street Map ID
#' values, and longitude and latitude values, and four variables:
#' \itemize{
#' \item 'car_t' Times in seconds to travel with automobile.
#' \item 'mm_t' Times in seconds for equivalent multi-modal transport.
#' \item 'walk_d' Equivalent walking distance in kilometres.
#' \item 'ratio' Ratio of multi-modal to automobile travel times.
#' }
#'
#' @examples
#' \dontrun{
#' city <- "<city_name>"
#' net <- m4ra_load_cached_network (city = city, mode = "foot")
#' v <- m4ra_vertices (net, "<city_name>")
#' from <- sample (v$id, size = 10)
#' dat <- m4ra_mm_car_ratios (city = city, from = from)
#' }
#' @family analyses
#' @export

m4ra_times_mm_car <- function (net_sc = NULL,
                               gtfs = NULL,
                               city_name = NULL,
                               day = NULL,
                               start_time_limits = NULL,
                               initial_mode = "foot",
                               final_mode = "foot",
                               from = NULL,
                               duration_max = NULL,
                               fast = FALSE,
                               n_closest = 10L,
                               walk_dists = TRUE,
                               quiet = FALSE) {

    requireNamespace ("dplyr")

    if (is.null (from)) {
        stop ("'from' must be specified")
    }
    checkmate::assert_character (from, min.len = 1L)
    checkmate::assert_character (city_name, max.len = 1L)
    city <- gsub ("\\s+", "-", tolower (city_name))

    v_f <- NULL
    if (walk_dists) {
        graph_f <-
            m4ra_load_cached_network (city, mode = "foot", contracted = TRUE)
        v_f <- m4ra_vertices (graph_f, city)
    }

    graph_b <-
        m4ra_load_cached_network (city, mode = "bicycle", contracted = TRUE)
    v_b <- m4ra_vertices (graph_b, city)
    rm (graph_b)

    graph_c <-
        m4ra_load_cached_network (city, mode = "motorcar", contracted = TRUE)
    v_c <- m4ra_vertices (graph_c, city)

    # Need to remove 'wt_profile_file' attribute, as turn angles are already
    # included within the graph:
    attr (graph_c, "wt_profile_file") <- NULL

    if (!quiet) {
        message ("Calculating times from [", length (from), "] vertices")
    }

    v <- rbind (v_f, v_b, v_c)
    ids <- unique (v$id)
    v <- v [match (ids, v$id), ]

    # match "from" points on to nearest pts in car network. This uses
    # 'match_points_to_verts', because don't know at this stage the network from
    # which those points are drawn, plus there'll generally be relatively few of
    # them.
    v_from <- v [match (from, v$id), ]
    from_car <-
        v_c$id [dodgr::match_points_to_verts (v_c, v_from [, c ("x", "y")])]
    car_times <- m4ra_times_single_mode (graph_c, from = from_car)
    # dim: (nfrom, nverts)
    rm (graph_c)

    car_times <- add_parking_times (car_times, v_c, city)

    # Then re-map car_times onto vertices of "final mode" graph:
    if (final_mode == "foot") {
        v_final <- v_f
        index <- load_vert_index (city, mode1 = "motorcar", mode2 = "foot")
        index_walk <- seq_len (nrow (v_final))
    } else {
        v_final <- v_b
        index <- load_vert_index (city, mode1 = "motorcar", mode2 = "bicycle")
        index_walk <- load_vert_index (city, mode1 = "foot", mode2 = "bicycle")
    }
    car_times <- car_times [, index, drop = FALSE]
    colnames (car_times) <- v_final$id

    # match "from" points on to nearest pts in 'initial_mode' network:
    if (initial_mode == "foot") {
        initial_verts <- v_f
    } else if (initial_mode == "bicycle") {
        initial_verts <- v_b
    }

    from_initial <- initial_verts$id [
        dodgr::match_points_to_verts (initial_verts, v_from [, c ("x", "y")])
    ]
    mm_times <- m4ra_times_multi_mode (
        net_sc = net_sc,
        gtfs = gtfs,
        city_name = city,
        day = day,
        start_time_limits = start_time_limits,
        initial_mode = initial_mode,
        final_mode = final_mode,
        from = from_initial,
        duration_max = duration_max,
        fast = fast,
        n_closest = n_closest,
        quiet = quiet
    )

    if (walk_dists) {
        # Still have to re-calculate even if initial_mode = "foot", because
        # these are distances, not times.
        from_foot <-
            v_f$id [dodgr::match_points_to_verts (v_f, v_from [, c ("x", "y")])]
        walk_d <- dodgr::dodgr_distances (graph_f, from = from_foot)
        walk_d <- walk_d [, index_walk, drop = FALSE]
        colnames (walk_d) <- v_final$id
        rm (graph_f)

        walk_d <- walk_d / 1000
    }

    # Finally, 'mm_times' auto-removes any vertices which are unreachable, so
    # reduce all matrices to the size of this:
    index <- match (colnames (mm_times$times), colnames (car_times))
    car_times <- car_times [, index, drop = FALSE]
    v_final <- v_final [index, ]
    if (walk_dists) {
        walk_d <- walk_d [, index, drop = FALSE]
    }

    ratio <- mm_times$times / car_times

    return (list (
        dist = walk_d,
        car_times = car_times,
        mm_times = mm_times$times,
        mm_transfers = mm_times$transfers,
        mm_intervals = mm_times$intervals,
        ratio = ratio,
        verts = v_final,
        v_from = v [match (from, v$id), ]
    ))
}

#' Add start and end time penalties for parking, only reading directly from
#' cached parking file if it exists
#' @noRd
add_parking_times <- function (car_times, verts_car, city) {

    cache_dir <- fs::path (m4ra_cache_dir (), city)
    f_parking <- fs::dir_ls (cache_dir, regexp = "parking", fixed = TRUE)

    if (length (f_parking) > 0) {
        f_parking_hash <- substring (digest::digest (verts_car$id), 1, 6)
        f_parking <- grep (f_parking_hash, f_parking, value = TRUE)
    }

    if (length (f_parking == 1L)) {

        parking <- m_readRDS (f_parking)
        index <- match (rownames (car_times), parking$id)
        p_start_mat <- array (
            parking$penalty_start [index],
            dim = dim (car_times)
        )
        index <- match (colnames (car_times), parking$id)
        p_end_mat <- t (array (
            parking$penalty_start [index],
            dim = rev (dim (car_times))
        ))
        car_times <- p_start_mat + car_times + p_end_mat
    }

    return (car_times)
}
