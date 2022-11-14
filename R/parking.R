
#' Get data on parking availability to estimate time penalties for automobile
#' travel.
#'
#' @param city_name Name of city used to name cached files.
#' @param bb Bounding box of city for query to extract parking data.
#' @param mode Mode of transport used to extract OSM node IDs at which to
#' estimate relative parking availability.
#' @return A `data.frame` of the vertices of the (contracted) network, with
#' additional columns quantifying number of parking spaces associated with each
#' vertex, as well as the total volume of all surrounding buildings.
#' @export
m4ra_parking <- function (bb, city_name, mode = "foot") {

    requireNamespace ("dplyr")
    requireNamespace ("osmdata")
    requireNamespace ("sf")

    parking <- get_parking_data (bb)
    buildings <- get_building_data (bb)

    graph <- m4ra_load_cached_network (city = city_name, mode = mode)
    graph_c <- dodgr::dodgr_contract_graph (graph)
    graph_c <- graph_c [graph_c$component == 1L, ]
    v <- dodgr::dodgr_vertices (graph_c)

    from <- v$id

    # Aggregate parking at each node of `graph_c`:
    index <- dodgr::match_points_to_verts (v, sf::st_coordinates (parking))
    parking$osm_id <- v$id [index]
    xy <- sf::st_coordinates (parking)
    parking <- sf::st_drop_geometry (parking)
    parking$x <- xy [, 1]
    parking$y <- xy [, 2]
    parking <- parking [which (is.finite (parking$capacity) & !is.na (parking$capacity)), ]

    parking <- dplyr::group_by (parking, osm_id) |>
        dplyr::summarise (
            x = x [1],
            y = y [1],
            capacity = sum (capacity)
        )

    capacity <- parking [["capacity"]]
    to <- parking$osm_id

    graph_c <- preprocess_spatial_cols (graph_c)
    gr_cols <- dodgr_graph_cols (graph_c)
    is_spatial <- is_graph_spatial (graph_c)
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
        dlim = 5000
    )

    capacity <- d [, 2] / d [, 1]
    # Then re-scale to total actual capacity times relative difference in
    # numbers of nodes
    rel_nodes <- nrow (v) / nrow (parking)
    capacity <- capacity * rel_nodes * sum (parking$capacity, na.rm = TRUE) /
        sum (capacity, na.rm = TRUE)

    # Then the same for buildings
    index <- dodgr::match_points_to_verts (v, sf::st_coordinates (buildings))
    buildings$osm_id <- v$id [index]
    xy <- sf::st_coordinates (buildings)
    buildings <- sf::st_drop_geometry (buildings)
    buildings$x <- xy [, 1]
    buildings$y <- xy [, 2]
    buildings <- buildings [which (is.finite (buildings$volume) & !is.na (buildings$volume)), ]

    buildings <- dplyr::group_by (buildings, osm_id) |>
        dplyr::summarise (
            x = x [1],
            y = y [1],
            volume = sum (volume)
        )

    volume <- buildings [["volume"]]
    to <- buildings$osm_id

    to_from_indices <- to_from_index_with_tp (graph_c, from, to)

    vol_wt <- rcpp_weighted_dists (
        graph_c,
        to_from_indices$vert_map,
        to_from_indices$from$index,
        to_from_indices$to$index,
        volume,
        dlim = 5000
    )
    volume <- vol_wt [, 2] / vol_wt [, 1]
    rel_nodes <- nrow (v) / nrow (buildings)
    volume <- volume * rel_nodes * sum (buildings$volume, na.rm = TRUE) /
        sum (volume, na.rm = TRUE)

    v$parking <- capacity
    v$building_volume <- volume

    return (v)
}

#' Centroids of all parking polygons and points, and associated capacities.
#' @noRd
get_parking_data <- function (bb) {


    # suppress no visible binding notes:
    amenity <- parking <- NULL

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

    combine_pts_and_polys <- function (dat) {

        cols0 <- c ("osm_id", "amenity", "building", "parking", "capacity", "geometry")
        cols <- cols0 [which (cols0 %in% names (dat$osm_points))]
        dat_pts <- dat$osm_points [grep ("parking", dat$osm_points$amenity), cols]

        cols <- cols0 [which (cols0 %in% names (dat$osm_polygons))]
        dat_poly <- dat$osm_polygons [grep ("parking", dat$osm_polygons$amenity), cols]

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
    dat_p$capacity <- as.integer (dat_p$capacity)

    # Then replace all NA "capacity" values by mean values for each pair of
    # "amenity" and "parking" key-values:
    dat_p <- dplyr::group_by (dat_p, amenity, parking) |>
        dplyr::mutate_at("capacity", function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)))

    return (dat_p)
}

#' Total volumes of all buildings
#' @noRd
get_building_data <- function (bb) {

    # suppress no visible binding notes:
    building <- NULL

    dat_b <- osmdata::opq (bb) |>
        osmdata::add_osm_feature (key = "building") |>
        osmdata::osmdata_sf (quiet = FALSE)

    cols <- c ("building", "height")
    p <- dat_b$osm_polygons [, c (cols, "geometry")]
    index <- grep ("ft$|feet$", p$height)
    p$height [index] <- as.numeric (gsub ("ft$|feet$", "", p$height [index])) * 12 * 0.0254
    p$height <- as.numeric (gsub ("\\s?m$", "", p$height))

    p <- dplyr::group_by (p, building) |>
        dplyr::mutate_at ("height", function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)))
    p$area <- sf::st_area (p)
    p$volume <- p$area * p$height
    p <- sf::st_centroid (p)

    return (p)
}
