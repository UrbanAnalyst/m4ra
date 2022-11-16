
#' Get data on parking availability and building volumes to estimate time
#' penalties for automobile travel.
#'
#' @param city_name Name of city used to name cached files.
#' @param bb Bounding box of city for query to extract parking data.
#' @param mode Mode of transport used to extract OSM node IDs at which to
#' estimate relative parking availability.
#' @param planet_file Optional file path to local `.osm.pbf` or `.osm.bz2` file
#' encompassing specified bounding box. If given, data are extracted with
#' system-level calls to "osmium", which must be installed.
#' @param dlim Distance limit in metres out to which contributions of parking
#' and buildings will be aggregated.
#' @param k With of exponential function used to weight contributions with
#' distance, \code{exp(-d / k)} for distance `d`. Default value of 1000 metres
#' decreases weight to 37% at 1km, 14% at 2km, and 0.6% at 5km.
#' @param quiet If `FALSE`, display progress information on screen.
#' @return A `data.frame` of the vertices of the (contracted) network, with
#' additional columns quantifying number of parking spaces associated with each
#' vertex, as well as the total volume of all surrounding buildings.
#'
#' @family analyses
#' @export
m4ra_parking <- function (bb, city_name, mode = "foot",
                          planet_file = NULL, dlim = 5000, k = 1000,
                          quiet = FALSE) {

    requireNamespace ("dplyr")
    requireNamespace ("osmdata")
    requireNamespace ("sf")

    parking <- get_parking_data (bb, planet_file, city_name, quiet)
    buildings <- get_building_data (bb, planet_file, city_name, quiet)

    graph <- m4ra_load_cached_network (city = city_name, mode = mode)
    graph_c <- dodgr::dodgr_contract_graph (graph)
    graph_c <- graph_c [graph_c$component == 1L, ]

    parking <- aggregate_parking_data (graph_c, parking, dlim)
    buildings <- aggregate_building_data (graph_c, buildings, dlim)

    v$parking <- parking
    v$building_volume <- buildings

    # The final ratio is then number of parking spaces divided by the cubic root
    # of the buildnig volume.
    v$ratio <- v$parking / as.numeric (v$building_volume) ^ (1 / 3)

    return (v)
}

#' Get raw Open Street Map parking data.
#'
#' Centroids of all parking polygons and points, and associated capacities.
#' @noRd
get_parking_data <- function (bb, planet_file = NULL, city_name, quiet = FALSE) {

    # suppress no visible binding notes:
    amenity <- parking <- NULL

    if (!is.null (planet_file)) {

        osm_files <- osmium_process (planet_file, bb, city_name, quiet = quiet)

        # key = "parking":
        f_p <- grep ("parking\\.", osm_files, value = TRUE)
        dat_p <- osmdata::opq (bb) |>
            osmdata::add_osm_feature (key = "parking") |>
            osmdata::osmdata_sf (doc = f_p, quiet = FALSE)

        # key = "amenity" or "building":
        f_a <- grep ("amenity", osm_files, value = TRUE)
        dat_a <- osmdata::opq (bb) |>
            osmdata::add_osm_features (features = c (
                "\"amentiy\"=\"parking\"",
                "\"building\"=\"garage\"",
                "\"building\"=\"garages\""
            )) |>
            osmdata::osmdata_sf (doc = f_a, quiet = FALSE)

    } else {

        # key = "parking":
        dat_p <- osmdata::opq (bb) |>
            osmdata::add_osm_feature (key = "parking") |>
            osmdata::osmdata_sf (quiet = FALSE)

        # key = "amenity" or "building":
        dat_a <- osmdata::opq (bb) |>
            osmdata::add_osm_features (features = c (
                "\"amentiy\"=\"parking\"",
                "\"building\"=\"garage\"",
                "\"building\"=\"garages\""
            )) |>
            osmdata::osmdata_sf (quiet = FALSE)
    }

    combine_pts_and_polys <- function (dat) {

        cols0 <- c ("osm_id", "amenity", "building",
            "parking", "capacity", "geometry")
        cols <- cols0 [which (cols0 %in% names (dat$osm_points))]
        dat_pts <-
            dat$osm_points [grep ("parking", dat$osm_points$amenity), cols]

        cols <- cols0 [which (cols0 %in% names (dat$osm_polygons))]
        dat_poly <-
            dat$osm_polygons [grep ("parking", dat$osm_polygons$amenity), cols]

        # Rm any nodes that are part of those polygons
        ids <- length (unique (names (unlist (dat_poly$geometry))))
        dat_pts <- dat_pts [which (!dat_pts$osm_id %in% ids), ]
        dat_poly <- sf::st_centroid (dat_poly)

        dat <- dplyr::bind_rows (dat_pts, dat_poly)

        return (dat)
    }

    dat_p <- combine_pts_and_polys (dat_p)
    dat_a <- combine_pts_and_polys (dat_a)
    dat_a <- dat_a [which (!dat_a$osm_id %in% dat_p$osm_id), ]
    dat_p <- dplyr::bind_rows (dat_p, dat_a)

    not_these <- c (
        "parking_entrance",
        "bicycle_parking",
        "motorcycle_parking"
    )
    dat_p <- dat_p [which (!dat_p$amenity %in% not_these), ]

    # Process "capacity" values first by taking mean of any ranges:
    index <- grep ("\\-", dat_p$capacity)
    if (length (index) > 0) {
        cap <- sapply (strsplit (dat_p$capacity [index], "-"), function (i)
                       mean (as.integer (i)))
        dat_p$capacity [index] <- round (cap)

    }
    # And remove everything except integers:
    dat_p$capacity <- as.integer (gsub ("[^(0-9)+]", "", dat_p$capacity))

    # Then replace all NA "capacity" values by mean values for each pair of
    # "amenity" and "parking" key-values:
    dat_p <- dplyr::group_by (dat_p, amenity, parking) |>
        dplyr::mutate_at("capacity", function(x) {
            replace(x, is.na(x), mean(x, na.rm = TRUE))
        })

    return (dat_p)
}

#' Get raw Open Street Map building data
#'
#' Total volumes of all buildings
#' @noRd
get_building_data <- function (bb, planet_file, city_name, quiet = FALSE) {

    # suppress no visible binding notes:
    building <- NULL

    if (!is.null (planet_file)) {

        osm_files <- osmium_process (planet_file, bb, city_name, quiet = quiet)

        f_b <- grep ("building", osm_files, value = TRUE)
        dat_b <- osmdata::opq (bb) |>
            osmdata::add_osm_feature (key = "building") |>
            osmdata::osmdata_sf (doc = f_b, quiet = FALSE)
    
    } else {

        dat_b <- osmdata::opq (bb) |>
            osmdata::add_osm_feature (key = "building") |>
            osmdata::osmdata_sf (quiet = FALSE)
    }

    cols <- c ("building", "height")
    p <- dat_b$osm_polygons [, c (cols, "geometry")]
    index <- grep ("ft$|feet$", p$height)
    p$height [index] <-
        as.numeric (gsub ("ft$|feet$", "", p$height [index])) * 12 * 0.0254
    # Then remove everything that is not numeric:
    p$height <- gsub ("[^(0-9)+(?\\.(0-9)+)]", "", p$height)
    p$height <- as.numeric (p$height)

    p <- dplyr::group_by (p, building) |>
        dplyr::mutate_at ("height", function(x) {
            replace(x, is.na(x), mean(x, na.rm = TRUE))
        })
    p$area <- sf::st_area (p)
    p$volume <- p$area * p$height
    p <- sf::st_centroid (p)

    return (p)
}

#' Aggregate raw parking data to each vertex of a contracted graph.
#' @noRd
aggregate_parking_data <- function (graph_c, parking, dlim = 5000, k = 1000) {

    # suppress no visible binding notes:
    osm_id <- x <- y <- NULL

    v <- dodgr::dodgr_vertices (graph_c)

    from <- v$id

    # Aggregate parking at each node of `graph_c`:
    index <- dodgr::match_points_to_verts (v, sf::st_coordinates (parking))
    parking$osm_id <- v$id [index]
    xy <- sf::st_coordinates (parking)
    parking <- sf::st_drop_geometry (parking)
    parking$x <- xy [, 1]
    parking$y <- xy [, 2]
    parking <- parking [
        which (is.finite (parking$capacity) & !is.na (parking$capacity)),
    ]

    parking <- dplyr::group_by (parking, osm_id) |>
        dplyr::summarise (
            x = x [1],
            y = y [1],
            capacity = sum (capacity)
        )

    capacity <- parking [["capacity"]]
    to <- parking$osm_id

    graph_c <- preprocess_spatial_cols (graph_c)
    to_from_indices <- to_from_index_with_tp (graph_c, from, to)
    if (to_from_indices$compound) {
        graph_c <- to_from_indices$graph_compound
    }

    d <- rcpp_weighted_dists (
        graph_c,
        to_from_indices$vert_map,
        to_from_indices$from$index,
        to_from_indices$to$index,
        capacity,
        dlim = dlim,
        k = k
    )

    capacity <- d [, 2] / d [, 1]
    # Then re-scale to total actual capacity times relative difference in
    # numbers of nodes
    rel_nodes <- nrow (v) / nrow (parking)
    capacity <- capacity * rel_nodes * sum (parking$capacity, na.rm = TRUE) /
        sum (capacity, na.rm = TRUE)

    return (capacity)
}

aggregate_building_data <- function (graph_c, buildings,
                                     dlim = 5000, k = 1000) {

    # suppress no visible binding notes:
    osm_id <- x <- y <- from <- NULL

    v <- dodgr::dodgr_vertices (graph_c)

    from <- v$id

    index <- dodgr::match_points_to_verts (v, sf::st_coordinates (buildings))
    buildings$osm_id <- v$id [index]
    xy <- sf::st_coordinates (buildings)
    buildings <- sf::st_drop_geometry (buildings)
    buildings$x <- xy [, 1]
    buildings$y <- xy [, 2]
    buildings <- buildings [
        which (is.finite (buildings$volume) & !is.na (buildings$volume)),
    ]

    buildings <- dplyr::group_by (buildings, osm_id) |>
        dplyr::summarise (
            x = x [1],
            y = y [1],
            volume = sum (volume)
        )

    volume <- buildings [["volume"]]
    to <- buildings$osm_id

    graph_c <- preprocess_spatial_cols (graph_c)
    to_from_indices <- to_from_index_with_tp (graph_c, from, to)
    if (to_from_indices$compound) {
        graph_c <- to_from_indices$graph_compound
    }

    vol_wt <- rcpp_weighted_dists (
        graph_c,
        to_from_indices$vert_map,
        to_from_indices$from$index,
        to_from_indices$to$index,
        volume,
        dlim = dlim,
        k = k
    )
    volume <- vol_wt [, 2] / vol_wt [, 1]
    rel_nodes <- nrow (v) / nrow (buildings)
    volume <- volume * rel_nodes * sum (buildings$volume, na.rm = TRUE) /
        sum (volume, na.rm = TRUE)

    return (volume)
}
