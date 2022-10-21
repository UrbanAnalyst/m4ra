#' Calculate times from a specified point by bicycle and automobile, along with
#' corresponding walking distances.
#'
#' This city expects weighted networks to have been generated with the 
#' \link{m4ra_batch_weight_networks} function, and for corresponding networks to
#' exist in the `m4ra` cache directory for the specified city.
#'
#' @param city City for which values are to be calculated
#' @param from Vector of OSM ID values of vertices from which ratios are to be
#' calculated. Typically obtained by loading one weighted network, and
#' sampling or extracting vertices from the \pkg{dodgr} function
#' `dodgr_vertices`.
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
#' v <- dodgr::dodgr_vertices (net)
#' from <- sample (v$id, size = 10)
#' dat <- m4ra_bike_car_ratios (city = city, from = from)
#' }
#' @family analyses
#' @export

m4ra_bike_car_times <- function (city = NULL, from = NULL) {

    requireNamespace ("dplyr")

    # suppress no visible binding notes:
    bike_t <- car_t <- walk_d <- ratio <- NULL

    graph_f <- m4ra_load_cached_network (city, mode = "foot")
    graph_f <- graph_f [graph_f$component == 1, ]
    v_f <- dodgr::dodgr_vertices (graph_f)
    graph_b <- m4ra_load_cached_network (city, mode = "bicycle")
    graph_b <- graph_b [graph_b$component == 1, ]
    v_b <- dodgr::dodgr_vertices (graph_b)
    graph_c <- m4ra_load_cached_network (city, mode = "motorcar")
    graph_c <- graph_c [graph_c$component == 1, ]
    v_c <- dodgr::dodgr_vertices (graph_c)

    if (is.null (from)) {
        # Get vertices common to all networks:
        vert_count <- table (c (
            unique (graph_f$.vx0),
            unique (graph_b$.vx0),
            unique (graph_c$.vx0)
        ))
        verts_all <- names (vert_count) [which (vert_count == 3)]
        v <- v_f [which (v_f$id %in% verts_all), ]

        from <- v$id
        message ("Calculating times from [", length (from), "] vertices")
    }

    v <- rbind (v_f, v_b, v_c)
    ids <- unique (v$id)
    v <- v [match (ids, v$id), ]

    car_times <- m4ra_times_single_mode (graph_c, from = from)
    car_times <- data.frame (t (car_times))
    car_times <- cbind (id = rownames (car_times), car_times)

    bike_times <- m4ra_times_single_mode (graph_b, from = from)
    bike_times <- data.frame (t (bike_times))
    bike_times <- cbind (id = rownames (bike_times), bike_times)

    walk_dists <- dodgr::dodgr_distances (graph_f, from = from)
    walk_dists <- data.frame (t (walk_dists))
    walk_dists <- cbind (id = rownames (walk_dists), walk_dists)

    ids <- table (c (car_times$id, bike_times$id, walk_dists$id))
    ids <- names (ids) [which (ids == 3)]
    car_times <- car_times [match (ids, car_times$id), -1]
    bike_times <- bike_times [match (ids, bike_times$id), -1]
    walk_dists <- walk_dists [match (ids, walk_dists$id), -1] / 1000

    ratio <- bike_times / car_times
    v <- v [match (ids, v$id), ]

    return (list (
        dist = walk_dists,
        ratio = ratio,
        verts = v
    ))
}

#' Convert bicycle and automobile times into equivalent areas for a specified
#' range of ratios of the two travel times.
#'
#' @param bike_car_dat Result of \link{m4ra_bike_car_times} function for a
#' specified vector of `from` points.
#' @param ratio_lims Vector of ratio limits used to calculate areas within which
#' ratio of bicycle to automobile times is less than specified limits.
#' @return A `data.frame` with one row for each `from` point used to calculate
#' \link{m4ra_bike_car_times}, and columns including the OSM "id" value and
#' corresponding coordinates ("x" and "y"), followed by a series of columns, one
#' for each specified value of `ratio_lims`, containing the areas in square
#' kilometres over which bicycle travel times are within the specified ratio of
#' car travel times.
#' @family analyses
#' @export
m4ra_bike_car_ratio_areas <- function (bike_car_dat, ratio_lims = 1 + 0:10 / 10) {

    requireNamespace ("geosphere")

    ratio_lims <- ratio_lims [which (ratio_lims < max (bike_car_dat$ratio, na.rm = TRUE))]

    # Convex hull area (in square km) for one ratio:
    ratio_area <- function (ratios, verts, ratio_lim = 1.0) {

        xy <- as.matrix (verts [which (ratios <= ratio_lim), c ("x", "y")])
        xy <- xy [chull (xy), , drop = FALSE]
        area <- 0
        if (nrow (xy) > 3L) {
            area <- geosphere::areaPolygon (xy) / 1e6
        }
        return (area)
    }

    nverts <- ncol (bike_car_dat$ratio)
    areas <- lapply (seq_len (nverts), function (i) {
        vapply (ratio_lims, function (r)
            ratio_area (bike_car_dat$ratio [, i], bike_car_dat$verts, r),
            numeric (1L))
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
