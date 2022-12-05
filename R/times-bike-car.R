#' Calculate relative times from a specified point by bicycle versus private
#' automobile, along with (optionally) corresponding walking distances.
#'
#' This city expects weighted networks to have been generated with the
#' \link{m4ra_batch_weight_networks} function, and for corresponding networks to
#' exist in the `m4ra` cache directory for the specified city.
#'
#' @param city City for which values are to be calculated
#' @param from Vector of OSM ID values of vertices from which ratios are to be
#' calculated. Typically obtained by loading one weighted network, and
#' sampling or extracting vertices from the function
#' `m4ra_vertices`.
#' @param walk_dists If `TRUE`, also calculate equivalent walking distances.
#' @return A `data.frame` of destination vertices, including Open Street Map ID
#' values, and longitude and latitude values, and four variables:
#' \itemize{
#' \item 'car_t' Times in seconds to travel with automobile.
#' \item 'bike_t' Times in seconds to travel by bicycles.
#' \item 'walk_d' Equivalent walking distance in kilometres.
#' \item 'ratio' Ratio of bicycle to automobile travel times.
#' }
#'
#' @examples
#' \dontrun{
#' city <- "<city_name>"
#' net <- m4ra_load_cached_network (city = city, mode = "foot")
#' v <- m4ra_vertices (net, "<city_name>")
#' from <- sample (v$id, size = 10)
#' dat <- m4ra_bike_car_ratios (city = city, from = from)
#' }
#' @family analyses
#' @export

m4ra_times_bike_car <- function (city = NULL, from = NULL, walk_dists = TRUE) {

    requireNamespace ("dplyr")

    if (is.null (from)) {
        stop ("'from' must be specified")
    }
    checkmate::assert_character (from, min.len = 1L)
    checkmate::assert_character (city, max.len = 1L)
    city <- gsub ("\\s+", "-", tolower (city))

    # suppress no visible binding notes:
    bike_t <- car_t <- walk_d <- ratio <- NULL
    graph_f <- v_f <- NULL

    if (walk_dists) {
        graph_f <- m4ra_load_cached_network (city, mode = "foot", contracted = TRUE)
        graph_f <- graph_f [graph_f$component == 1, ]
        v_f <- m4ra_vertices (graph_f, city)
    }
    graph_b <- m4ra_load_cached_network (city, mode = "bicycle", contracted = TRUE)
    graph_b <- graph_b [graph_b$component == 1, ]
    v_b <- m4ra_vertices (graph_b, city)
    graph_c <- m4ra_load_cached_network (city, mode = "motorcar", contracted = TRUE)
    graph_c <- graph_c [graph_c$component == 1, ]
    v_c <- m4ra_vertices (graph_c, city)

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

    car_times <- add_parking_times (car_times, v_c, city)

    car_times <- data.frame (t (car_times))
    car_times <- cbind (id = rownames (car_times), car_times)

    # match "from" points on to nearest pts in bike network:
    from_bike <- v_b$id [dodgr::match_points_to_verts (v_b, v_from [, c ("x", "y")])]
    bike_times <- m4ra_times_single_mode (graph_b, from = from_bike)
    bike_times <- data.frame (t (bike_times))
    bike_times <- cbind (id = rownames (bike_times), bike_times)

    all_ids <- c (bike_times$id, car_times$id)
    if (walk_dists) {
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
    bike_times <- bike_times [match (ids, bike_times$id), -1, drop = FALSE]
    if (walk_dists) {
        walk_d <- walk_d [match (ids, walk_d$id), -1, drop = FALSE] / 1000
    }

    ratio <- bike_times / car_times
    v <- v [match (ids, v$id), , drop = FALSE]

    return (list (
        dist = walk_d,
        ratio = ratio,
        verts = v
    ))
}

#' Convert bicycle and automobile times into equivalent areas for a specified
#' range of ratios of the two travel times.
#'
#' @param bike_car_dat Result of \link{m4ra_times_bike_car} function for a
#' specified vector of `from` points.
#' @param ratio_lims Vector of ratio limits used to calculate areas within which
#' ratio of bicycle to automobile times is less than specified limits.
#' @return A `data.frame` with one row for each `from` point used to calculate
#' \link{m4ra_times_bike_car}, and columns including the OSM "id" value and
#' corresponding coordinates ("x" and "y"), followed by a series of columns, one
#' for each specified value of `ratio_lims`, containing the areas in square
#' kilometres over which bicycle travel times are within the specified ratio of
#' car travel times.
#' @family analyses
#' @export
m4ra_bike_car_ratio_areas <- function (bike_car_dat,
                                       ratio_lims = 1 + 0:10 / 10) {

    requireNamespace ("geosphere")

    rmax <- max (bike_car_dat$ratio, na.rm = TRUE)
    ratio_lims <- ratio_lims [which (ratio_lims < rmax)]

    # Convex hull area (in square km) for one ratio:
    ratio_area <- function (ratios, verts, ratio_lim = 1.0) {

        xy <- as.matrix (verts [which (ratios <= ratio_lim), c ("x", "y")])
        xy <- xy [grDevices::chull (xy), , drop = FALSE]
        area <- 0
        if (nrow (xy) > 3L) {
            area <- geosphere::areaPolygon (xy) / 1e6
        }
        return (area)
    }

    nverts <- ncol (bike_car_dat$ratio)
    areas <- lapply (seq_len (nverts), function (i) {
        vapply (
            ratio_lims, function (r) {
                ratio_area (bike_car_dat$ratio [, i], bike_car_dat$verts, r) },
            numeric (1L)
        )
    })
    areas <- do.call (rbind, areas)

    # coordinates:
    from_names <- colnames (bike_car_dat$ratio)
    index <- match (gsub ("^X", "", from_names), bike_car_dat$verts$id)
    xy <- bike_car_dat$verts [index, ]
    xy <- xy [, which (!names (xy) %in% c ("component", "n"))]
    # then append ratios as columns onto that:
    n0 <- ncol (xy)
    dat <- cbind (xy, areas)
    cnames <- paste0 ("r", sprintf ("%1.1f", ratio_lims))
    index <- seq_len (ncol (dat)) [-seq_len (n0)]
    names (dat) [index] <- cnames

    return (dat)
}
