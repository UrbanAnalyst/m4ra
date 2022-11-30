
# Global variables defining time penalties for parking a car at the start and
# end of journeys. These are multiplied by the internal scale of the ratio of
# nearby parking capacity divided by the cubic root of nearby building volume.
# That ratio generally has mean and median values between 1 and 2, and maximal
# values around 4-5. These ratios are then converted to corresponding times in
# minutes by multiplying by the following factors, as documented in
# https://github.com/ATFutures/m4ra/issues/9
parking_ratio_multiplier_start <- 2
parking_ratio_multiplier_end <- 3

#' Get data on parking availability and building volumes to estimate time
#' penalties for automobile travel.
#'
#' @param city_name Name of city used to name cached files.
#' @param bb Bounding box of city for query to extract parking data.
#' @param mode Mode of transport used to extract OSM node IDs at which to
#' estimate relative parking availability (should generally be "motorcar").
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
#' @family prep
#' @export

m4ra_parking <- function (bb,
                          city_name,
                          mode = "motorcar",
                          planet_file = NULL,
                          dlim = 5000,
                          k = 1000,
                          quiet = FALSE) {

    city <- gsub ("\\s+", "-", tolower (city_name))

    graph_c <- m4ra_load_cached_network (
        city = city_name,
        mode = mode,
        contracted = TRUE
    )

    v <- m4ra_vertices (graph_c, city_name)

    cache_dir <- fs::path (m4ra_cache_dir (), city_name)

    v_hash <- substring (digest::digest (v$id), 1, 6)

    f_parking <- paste0 ("m4ra-", city, "-parking-", v_hash, ".Rds")
    f_parking <- file.path (cache_dir, f_parking)

    if (file.exists (f_parking)) {

        v <- readRDS (f_parking)

    } else {

        requireNamespace ("dplyr")
        requireNamespace ("osmdata")
        requireNamespace ("sf")

        parking <- get_parking_data (bb, planet_file, city_name, quiet)
        buildings <- get_building_data (bb, planet_file, city_name, quiet)

        v$parking <- aggregate_parking_data (
            graph_c,
            city_name,
            parking,
            dlim = dlim,
            k = k
        )
        v$building_volume <- aggregate_building_data (
            graph_c,
            city_name,
            buildings,
            dlim = dlim,
            k = k
        )

        # The final ratio is then number of parking spaces divided by the cubic
        # root of the building volume.
        v$ratio <- v$parking / as.numeric (v$building_volume)^(1 / 3)

        # And that is then converted to penalties at start and end, as
        # documented at top of this file.
        v$penalty_start <- v$ratio * parking_ratio_multiplier_start * 60
        v$penalty_end <- v$ratio * parking_ratio_multiplier_end * 60

        saveRDS (v, f_parking)
    }

    return (v)
}

#' Get raw Open Street Map parking data.
#'
#' Centroids of all parking polygons and points, and associated capacities.
#' @noRd
get_parking_data <- function (bb,
                              planet_file = NULL, city_name, quiet = FALSE) {

    # suppress no visible binding notes:
    amenity <- parking <- NULL

    if (!is.null (planet_file)) {

        osm_files <- osmium_process (planet_file, bb, city_name, quiet = quiet)

        # key = "parking":
        f_p <- grep ("parking\\.", osm_files, value = TRUE)
        dat_p <- osmdata::opq (bb) |>
            osmdata::add_osm_feature (key = "parking") |>
            osmdata::osmdata_sf (doc = f_p, quiet = quiet)

        # key = "amenity" or "building":
        f_a <- grep ("amenity", osm_files, value = TRUE)
        dat_a <- osmdata::opq (bb) |>
            osmdata::add_osm_features (features = c (
                "\"amentiy\"=\"parking\"",
                "\"building\"=\"garage\"",
                "\"building\"=\"garages\""
            )) |>
            osmdata::osmdata_sf (doc = f_a, quiet = quiet)

        # key = "parking:lane:<side>"
        f_l <- grep ("lane", osm_files, value = TRUE)
        dat_l <- osmdata::opq (bb) |>
            osmdata::add_osm_features (features = c (
                "\"parking:lane:left\"",
                "\"parking:lane:right\"",
                "\"parking:lane:both\""
            )) |>
            osmdata::osmdata_sf (doc = f_l, quiet = quiet)

    } else {

        # key = "parking":
        dat_p <- osmdata::opq (bb) |>
            osmdata::add_osm_feature (key = "parking") |>
            osmdata::osmdata_sf (quiet = quiet)

        # key = "amenity" or "building":
        dat_a <- osmdata::opq (bb) |>
            osmdata::add_osm_features (features = c (
                "\"amentiy\"=\"parking\"",
                "\"building\"=\"garage\"",
                "\"building\"=\"garages\""
            )) |>
            osmdata::osmdata_sf (quiet = quiet)

        dat_l <- osmdata::opq (bb) |>
            osmdata::add_osm_features (features = c (
                "\"parking:lane:left\"",
                "\"parking:lane:right\"",
                "\"parking:lane:both\""
            )) |>
            osmdata::osmdata_sf (quiet = quiet)
    }

    combine_pts_and_polys <- function (dat) {

        cols0 <- c (
            "osm_id", "amenity", "building",
            "parking", "capacity", "geometry"
        )
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
        cap <- sapply (
            strsplit (dat_p$capacity [index], "-"),
            function (i) mean (as.integer (i))
        )
        dat_p$capacity [index] <- round (cap)

    }
    # And remove everything except integers:
    dat_p$capacity <- as.integer (gsub ("[^(0-9)+]", "", dat_p$capacity))

    # Then replace all NA "capacity" values by mean values for each pair of
    # "amenity" and "parking" key-values:
    dat_p <- dplyr::group_by (dat_p, amenity, parking) |>
        dplyr::mutate_at ("capacity", function (x) {
            replace (x, is.na (x), mean (x, na.rm = TRUE))
        })

    # Finally add any on-street parking denoted with "parking:lane:<side>" keys.
    # This is done by converting the ways/lines to a `dodgr` network, and
    # matching vertices.
    dat_l <- process_onstreet_lanes (dat_l)

    return (list (dat_p = dat_p, dat_l = dat_l))
}

#' Calculate on-street parking denoted with "parking:lane:\<side\>" keys.
#'
#' This is done by converting the ways/lines to a `dodgr` network, and
#' matching vertices.
#' @noRd
process_onstreet_lanes <- function (dat_l) {

    keep_cols <- grep ("^parking\\.lane", names (dat_l$osm_lines), value = TRUE)
    net <- dodgr::weight_streetnet (dat_l$osm_lines,
        wt_profile = 1,
        keep_cols = keep_cols
    )

    # All possible combinations of parking lane tags:
    cols <- expand.grid (
        c ("both", "left", "right"),
        c ("", "parallel", "diagonal", "perpendicular")
    )
    cols <- paste0 ("parking.lane.", paste0 (cols [, 1], ".", cols [, 2]))
    cols <- gsub ("\\.$", "", cols)
    cols <- cols [which (cols %in% keep_cols)]

    # Use the following translations (in metres), mostly from
    # https://en.wikipedia.org/wiki/Parking_space
    sizes <- c (
        parallel = 6.5,
        perpendicular = 3,
        diagonal = 4.75 # (par + perp) / 2
    )

    # First need to process the tags which just end in "<side>", and have values
    # identifying parking direction:
    col_side <- grep ("(both|left|right)$", cols, value = TRUE)
    for (co in col_side) {
        index <- which (!is.na (net [[co]]))
        col_sub <- net [[co]] [index]
        i_par <- which (col_sub == "parallel")
        i_per <- which (col_sub == "perpendicular")
        i_dia <- which (col_sub == "diagonal")
        col_sub [i_par] <- sizes [1]
        col_sub [i_per] <- sizes [2]
        col_sub [i_dia] <- sizes [3]
        # replace any other sizes with random samples from actual distribution:
        index_in <- sort (c (i_par, i_per, i_dia))
        index_out <- seq_along (col_sub) [-index_in]
        values <- as.numeric (col_sub [index_in])
        col_sub [index_out] <-
            sample (values, size = length (index_out), replace = TRUE)
        net [[co]] [index] <- col_sub
        net [[co]] [which (is.na (net [[co]]))] <- 0
        net [[co]] <- as.numeric (net [[co]])

        if (co == "parking.lane.both") {
            net [[co]] <- 2 * net [[co]]
        }

        net [[co]] [index] <-
            as.integer (floor (net$d [index] / net [[co]] [index]))
    }

    # Other columns are then the explicit "parking:lane:<side>:<direction>"
    # tags. Key values for these don't matter, as they're mostly just
    # descriptions like "on_street" or "half_on_kerb".
    cols_dir <- cols [which (!cols %in% col_side)]
    for (co in cols_dir) {
        index <- which (!is.na (net [[co]]))
        net [[co]] [which (is.na (net [[co]]))] <- 0
        direction <- gsub ("^.*\\.", "", co)
        this_size <- sizes [match (direction, names (sizes))]
        net [[co]] [index] <- floor (net$d [index] / this_size)
        net [[co]] <- as.integer (net [[co]])

        if (grepl ("both", co)) {
            net [[co]] <- 2 * net [[co]]
        }
    }

    net$street_parking <- apply (net [, cols], 1, sum)

    net <- net [, c (
        "from_id", "from_lon", "from_lat",
        "to_id", "to_lon", "to_lat", "way_id", "street_parking"
    )]

    return (net)
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
            osmdata::osmdata_sf (doc = f_b, quiet = quiet)

    } else {

        dat_b <- osmdata::opq (bb) |>
            osmdata::add_osm_feature (key = "building") |>
            osmdata::osmdata_sf (quiet = quiet)
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
        dplyr::mutate_at ("height", function (x) {
            replace (x, is.na (x), mean (x, na.rm = TRUE))
        })
    p$area <- sf::st_area (p)
    p$volume <- p$area * p$height
    p <- sf::st_centroid (p)

    # Finally, exclude any buildings < 20 m ^ 2 or < 3 m high
    p <- p [which (as.numeric (p$area) > 20 & p$height > 3), ]

    return (p)
}

#' Aggregate raw parking data to each vertex of a contracted graph.
#'
#' @param parking A list of two objects: `dat_p` containing vertices and
#' capacities of parking from OSM nodes and polygons; and `dat_l` containing a
#' reduced `dodgr` street network with numbers of onstreet parking spaces
#' calculated in `process_onstreet_lanes()`.
#' @noRd
aggregate_parking_data <- function (graph_c, city, parking, dlim = 5000, k = 1000) {

    parking_lanes <- parking$dat_l
    parking <- parking$dat_p

    # suppress no visible binding notes:
    osm_id <- x <- y <- id <- NULL

    v <- m4ra_vertices (graph_c, city)

    from <- v$id

    # Aggregate parking from `dat_p` at each node of `graph_c`:
    index <- dodgr::match_points_to_verts (v, sf::st_coordinates (parking))
    parking$osm_id <- v$id [index]
    xy <- sf::st_coordinates (parking)
    parking <- sf::st_drop_geometry (parking)
    parking$x <- xy [, 1]
    parking$y <- xy [, 2]
    parking <- parking [
        which (is.finite (parking$capacity) & !is.na (parking$capacity)),
    ]
    parking <- parking [, c ("osm_id", "x", "y", "capacity")]

    # Then add the results of `process_onstreet_lanes`, allocating half of the
    # street parking to each terminal node.
    p_lanes <- data.frame (
        id = c (parking_lanes$from_id, parking_lanes$to_id),
        x = c (parking_lanes$from_lon, parking_lanes$to_lon),
        y = c (parking_lanes$from_lat, parking_lanes$to_lat),
        capacity = c (
            parking_lanes$street_parking,
            parking_lanes$street_parking
        ) / 2
    )
    p_lanes <- dplyr::group_by (p_lanes, id) |>
        dplyr::summarise (x = x [1], y = y [1], capacity = sum (capacity))
    index <- dodgr::match_points_to_verts (v, p_lanes [, c ("x", "y")])
    p_lanes$osm_id <- v$id [index]
    p_lanes$id <- NULL

    parking <- dplyr::bind_rows (parking, p_lanes)

    parking <- dplyr::group_by (parking, osm_id) |>
        dplyr::summarise (
            x = x [1],
            y = y [1],
            capacity = sum (capacity)
        )
    parking$capacity <- as.integer (round (parking$capacity))

    capacity <- parking [["capacity"]]
    to <- parking$osm_id

    attr (graph_c, "turn_penalty") <- 0
    to_from_indices <- to_from_index_with_tp (graph_c, from, to)

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

aggregate_building_data <- function (graph_c, city, buildings,
                                     dlim = 5000, k = 1000) {

    # suppress no visible binding notes:
    osm_id <- x <- y <- from <- NULL

    v <- m4ra_vertices (graph_c, city)

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
