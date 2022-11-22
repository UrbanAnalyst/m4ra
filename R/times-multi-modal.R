
#' Calculate matrix of pair-wise travel times between points using multiple
#' modes of transport.
#'
#' @param initial_mode Initial mode of transport from origin points towards
#' public transport stop.
#' @param from List of OSM vertex IDs from which to calculate total multi-modal
#' travel times. These must be vertices from the largest connected component of
#' the contracted graph.
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
                                   fast = FALSE,
                                   n_closest = 10L,
                                   quiet = FALSE) {

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
    graph_c <- graph_c [graph_c$component == 1L, ]
    v <- dodgr::dodgr_vertices (graph_c)
    if (!all (from %in% v$id)) {
        stop (
            "Not all specified 'from' vertices are part of the ",
            "largest connected component of the street network."
        )
    }

    stops <- m_readRDS (gtfs)$stops
    f <- grep ("gtfs\\-.*[0-9]{5}\\-[0-9]{5}\\.Rds$", files, value = TRUE)
    gtfs_mat <- m_readRDS (f)
    f <- grep ("gtfs\\-to\\-net", files, value = TRUE)
    gtfs_to_net <- m_readRDS (f)


    # need times to all vertices to extract overall minimal time from initial
    # mode vs GTFS-routed modes at end. The times here are then reduced to times
    # to the GTFS stops by matching vertices afterward.
    times <- m4ra_times_single_mode (graph_c, from = from)
    to <- v$id [dodgr::match_points_to_verts (
        v, stops [, c ("stop_lon", "stop_lat")])]
    times_to_gtfs <- times [, match (to, colnames (times)), drop = FALSE]

    # Then convert initial times to nearest GTFS stops to times through entire
    # GTFS network to all termimal network vertices.
    times [is.na (times)] <- -1
    gtfs_mat [is.na (gtfs_mat)] <- -1

    res <- rcpp_add_net_to_gtfs (
        times_to_gtfs,
        gtfs_mat,
        gtfs_to_net$index,
        gtfs_to_net$d,
        nrow (v)
    )

    maxr <- rcpp_matrix_max (res)
    res [res == maxr] <- NA

    # At that stage, `res` holds the fastest values routed through the GTFS
    # network. They just then have to be compared with the previously-calculated
    # times to all vertices using "initial_mode".

    res <- rcpp_min_from_two_matrices (res, times)

    rownames (res) <- from
    colnames (res) <- v$id

    return (res)
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
#' sampling or extracting vertices from the \pkg{dodgr} function
#' `dodgr_vertices`.
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
#' v <- dodgr::dodgr_vertices (net)
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

    if (walk_dists) {
        graph_f <- m4ra_load_cached_network (city, mode = "foot", contracted = TRUE)
        graph_f <- graph_f [graph_f$component == 1, ]
        v_f <- dodgr::dodgr_vertices (graph_f)
    }
    graph_b <- m4ra_load_cached_network (city, mode = "bicycle", contracted = TRUE)
    graph_b <- graph_b [graph_b$component == 1, ]
    v_b <- dodgr::dodgr_vertices (graph_b)
    graph_c <- m4ra_load_cached_network (city, mode = "motorcar", contracted = TRUE)
    graph_c <- graph_c [graph_c$component == 1, ]
    v_c <- dodgr::dodgr_vertices (graph_c)

    # Need to remove 'wt_profile_file' attribute, as turn angles are already
    # included within the graph:
    attr (graph_c, "wt_profile_file") <- NULL

    message ("Calculating times from [", length (from), "] vertices")

    v <- rbind (v_f, v_b, v_c)
    ids <- unique (v$id)
    v <- v [match (ids, v$id), ]

    # match "from" points on to nearest pts in car network:
    v_from <- v [match (from, v$id), ]
    from_car <- v_c$id [dodgr::match_points_to_verts (v_c, v_from [, c ("x", "y")])]
    car_times <- m4ra_times_single_mode (graph_c, from = from_car) # dim: (nfrom, nverts)

    # Add start and end time penalties for parking, only reading directly from
    # cached parking file if it exists
    cache_dir <- fs::path (m4ra_cache_dir (), city)
    f_parking <- list.files (cache_dir, full.names = TRUE, pattern = "parking")
    if (length (f_parking) > 0) {
        f_parking_hash <- substring (digest::digest (v_c$id), 1, 6)
        f_parking <- grep (f_parking_hash, f_parking, value = TRUE)
    }
    if (length (f_parking == 1L)) {
        parking <- m_readRDS (f_parking)
        index <- match (rownames (car_times), parking$id)
        p_start_mat <- array (parking$penalty_start [index], dim = dim (car_times))
        index <- match (colnames (car_times), parking$id)
        p_end_mat <- t (array (parking$penalty_start [index], dim = rev (dim (car_times))))
        car_times <- p_start_mat + car_times + p_end_mat
    }

    car_times <- data.frame (t (car_times))
    car_times <- cbind (id = rownames (car_times), car_times)

    # match "from" points on to nearest pts in 'initial_mode' network:
    if (initial_mode == "foot") {
        initial_verts <- v_f
        initial_graph <- graph_f
    } else if (initial_mode == "bicycle") {
        initial_verts <- v_b
        initial_graph <- graph_b
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
        fast = fast,
        n_closest = n_closest,
        quiet = quiet
    )
    mm_times <- data.frame (t (mm_times))
    mm_times <- cbind (id = rownames (mm_times), mm_times)

    all_ids <- c (initial_verts$id, car_times$id)

    if (walk_dists) {
        # Still have to re-calculate even if initial_mode = "foot", because
        # these are distances, not times.
        from_foot <- v_f$id [dodgr::match_points_to_verts (v_f, v_from [, c ("x", "y")])]
        walk_d <- dodgr::dodgr_distances (graph_f, from = from_foot)
        walk_d <- data.frame (t (walk_d))
        walk_d <- cbind (id = rownames (walk_d), walk_d)
        all_ids <- c (all_ids, walk_d$id)
    }

    ids <- table (all_ids)
    num_ids <- ifelse (walk_dists, 3L, 2L)
    ids <- names (ids) [which (ids == num_ids)]
    car_times <- car_times [match (ids, car_times$id), -1, drop = FALSE]
    mm_times <- mm_times [match (ids, mm_times$id), -1, drop = FALSE]
    if (walk_dists) {
        walk_d <- walk_d [match (ids, walk_d$id), -1, drop = FALSE] / 1000
    }

    ratio <- mm_times / car_times
    v <- v [match (ids, v$id), , drop = FALSE]

    return (list (
        dist = walk_d,
        ratio = ratio,
        verts = v
    ))
}
