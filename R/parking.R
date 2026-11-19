# Global variables defining time penalties for parking a car at the start and
# end of journeys. These are multiplied by the internal scale of the ratio of
# nearby parking capacity (numbers of parking spaces) divided by the cubic root
# of nearby building volume (in m^3, so the cubic root is in m). Both are
# distance-weighted means over all nearby parking sites or buildings, rescaled
# so that their mean over all vertices equals the overall mean capacity per
# parking site, and the overall mean volume per building (see
# `rescale_to_site_mean()`). That ratio generally has mean and median values
# between 1 and 2, and maximal values around 4-5. Maximal values are not
# limited by the rescaling, which only multiplies by constants, and may be
# higher where much parking is close to few or small buildings. These ratios are
# then converted to corresponding times in minutes by multiplying by the
# following factors, as documented in
# https://github.com/UrbanAnalyst/m4ra/issues/9, and the resulting penalties
# are then given in seconds (by multiplying by 60).
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
#' vertex (`parking`), as well as the volume of all surrounding buildings, in
#' cubic metres (`building_volume`). Both are distance-weighted means over
#' nearby parking sites and buildings, rescaled so that their means over all
#' vertices are the overall mean capacity per parking site, and mean volume per
#' building. The `ratio` column is `parking / building_volume ^ (1 / 3)`, and
#' `penalty_start` and `penalty_end` are the corresponding time penalties for
#' parking at the start and end of journeys, in seconds.
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

    cache_dir <- m4ra_cache_dir (city_name)

    v_hash <- substring (digest::digest (v$id), 1, 6)

    f_parking <- paste0 ("m4ra-", city, "-parking-", v_hash, ".Rds")
    f_parking <- fs::path (cache_dir, f_parking)

    if (fs::file_exists (f_parking)) {

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

        if (!quiet) {
            cli::cli_h3 (cli::col_green ("Extracting data on parking spaces:"))
        }

        osm_files <- osmium_process (planet_file, bb, city_name, quiet = quiet)

        use_osmextract <- function (f, extra_tags = "capacity") {
            list (
                osm_polygons = osmextract::oe_read (
                    f,
                    layer = "multipolygons",
                    extra_tags = extra_tags,
                    quiet = TRUE
                ) |> sf::st_cast ("POLYGON"),
                osm_points = osmextract::oe_read (
                    f,
                    layer = "points",
                    extra_tags = extra_tags,
                    quiet = TRUE
                )
            )
        }

        # key = "parking":
        tags <- c ("amenity", "building", "capacity", "parking")
        f_p <- grep ("parking\\.", osm_files, value = TRUE)
        dat_p <- use_osmextract (f_p, extra_tags = tags)

        # key = "amenity" or "building":
        f_a <- grep ("amenity", osm_files, value = TRUE)
        dat_a <- use_osmextract (f_a, extra_tags = tags)

        # key = "parking:lane:<side>"
        tags <- c (
            "parking",
            paste0 (
                "parking:lane:",
                c (
                    "", "both", "left", "right",
                    "parallel", "diagonal", "perpendicular"
                )
            )
        )
        f_l <- grep ("lane", osm_files, value = TRUE)
        dat_l <- osmextract::oe_read (
            f_l,
            layer = "lines",
            extra_tags = tags,
            quiet = TRUE
        )
        dat_l <- list (osm_lines = dat_l)

    } else {

        # key = "parking":
        dat_p <- osmdata::opq (bb) |>
            osmdata::add_osm_feature (key = "parking") |>
            osmdata::osmdata_sf (quiet = quiet)

        # key = "amenity" or "building":
        dat_a <- osmdata::opq (bb) |>
            osmdata::add_osm_features (features = c (
                "\"amenity\"=\"parking\"",
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

    process_parking_data (dat_p, dat_a, dat_l)
}

#' Process raw OSM parking data
#'
#' @param dat_p,dat_a Objects returned from `osmdata::osmdata_sf()` with
#' parking data from the key "parking" (`dat_p`), and from the key-value pairs
#' "amenity=parking" or "building=garage(s)" (`dat_a`).
#' @param dat_l List with a single `osm_lines` item with on-street parking
#' lanes from "parking:lane:<side>" keys.
#' @return A list of `dat_p` (points with capacities) and `dat_l` (a reduced
#' `dodgr` network with on-street parking numbers).
#' @noRd
process_parking_data <- function (dat_p, dat_a, dat_l) {

    # suppress no visible binding notes:
    amenity <- parking <- NULL

    # `sf` methods for subsetting must be registered:
    requireNamespace ("sf", quietly = TRUE)

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

    # Convert "capacity" values to numbers (see `parse_capacity()`):
    dat_p$capacity <- parse_capacity (dat_p$capacity)

    # Then replace all NA "capacity" values by mean values for each pair of
    # "amenity" and "parking" key-values. Groups with no capacity values at
    # all remain NA, and are removed when aggregating.
    dat_p <- dplyr::group_by (dat_p, amenity, parking) |>
        dplyr::mutate_at ("capacity", fill_na_with_mean) |>
        dplyr::ungroup ()

    # Finally add any on-street parking denoted with "parking:lane:<side>" keys.
    # This is done by converting the ways/lines to a `dodgr` network, and
    # matching vertices.
    dat_l <- process_onstreet_lanes (dat_l)

    return (list (dat_p = dat_p, dat_l = dat_l))
}

#' Convert OSM "capacity" values to numbers of parking spaces.
#'
#' - Plain numbers are returned as they are (with thousands separators such as
#' "1,000" removed).
#' - Ranges like "10-20" are converted to their (rounded) mean.
#' - Multiple values separated by ";" like "10;20" are summed, because these
#' generally describe separate parking areas.
#' - Anything else, including `NA`, is converted to the first number found in
#' the value (so "50 spaces" is 50), or to `NA` if there is none.
#'
#' @param x Character vector of OSM "capacity" values.
#' @return Numeric vector of the same length as `x`.
#' @noRd
parse_capacity <- function (x) {

    parse_one <- function (i) {
        i <- trimws (gsub ("(?<=[0-9]),(?=[0-9]{3}(\\D|$))", "", i, perl = TRUE))
        rng <- regmatches (i, regexec ("^([0-9]+)\\s*[-\u2013]\\s*([0-9]+)$", i)) [[1]]
        if (length (rng) == 3L) {
            return (round (mean (as.numeric (rng [2:3]))))
        }
        num <- regmatches (i, regexpr ("[0-9]+", i))
        ifelse (length (num) == 1L, as.numeric (num), NA_real_)
    }

    vapply (as.character (x), function (i) {
        if (is.na (i)) {
            return (NA_real_)
        }
        vals <- vapply (strsplit (i, ";") [[1]], parse_one, numeric (1L))
        ifelse (all (is.na (vals)), NA_real_, sum (vals, na.rm = TRUE))
    }, numeric (1L), USE.NAMES = FALSE)
}

#' Convert OSM building "height" values to metres.
#'
#' Values with "ft", "feet" or "'" are converted from feet to metres. Decimal
#' commas such as "12,5" are accepted. Where several values are given (for
#' example "10;12"), or other text is present, the first number is used. Values
#' with no number are `NA`.
#'
#' @param x Character vector of OSM "height" values.
#' @return Numeric vector of the same length as `x` (in metres).
#' @noRd
parse_height <- function (x) {

    x <- as.character (x)
    is_feet <- grepl ("(ft|feet|')\\s*$", x)
    x <- gsub ("([0-9]),([0-9])", "\\1.\\2", x)
    num <- regmatches (x, regexpr ("[0-9]+(\\.[0-9]+)?", x))
    h <- rep (NA_real_, length (x))
    h [grep ("[0-9]", x)] <- as.numeric (num)
    h [is_feet & !is.na (h)] <- h [is_feet & !is.na (h)] * 12 * 0.0254

    return (h)
}

#' Replace `NA` values by the mean of the other values, leaving `x` unchanged
#' if all values are `NA` (rather than replacing with `NaN`).
#' @noRd
fill_na_with_mean <- function (x) {

    if (all (is.na (x))) {
        return (x)
    }
    replace (x, is.na (x), mean (x, na.rm = TRUE))
}

#' Combine parking points and polygons from OSM data into a single `data.frame`
#' of points, with polygons reduced to their centroids.
#'
#' Any points which are vertices of the retained polygons are removed, so that
#' parking is not counted twice.
#'
#' @param dat Object returned from `osmdata::osmdata_sf()`.
#' @noRd
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
    # Only retain valid polygons:
    dat_poly <- dat_poly [which (sf::st_is_valid (dat_poly)), ]

    # Rm any nodes that are part of those polygons
    ids <- polygon_node_ids (dat_poly$geometry)
    dat_pts <- dat_pts [which (!dat_pts$osm_id %in% ids), ]
    dat_poly <- sf::st_centroid (dat_poly)

    dat <- dplyr::bind_rows (dat_pts, dat_poly)

    return (dat)
}

#' Get the OSM node IDs of all vertices of a list of polygons.
#'
#' `osmdata_sf()` polygons store node IDs as the row names of the coordinate
#' matrix of each ring.
#' @param geometry List of polygon or multipolygon geometries.
#' @return Character vector of unique node IDs.
#' @noRd
polygon_node_ids <- function (geometry) {

    ids <- lapply (geometry, function (g) {
        rapply (unclass (g), function (m) rownames (m), how = "unlist")
    })
    unique (as.character (unlist (ids)))
}

#' Calculate on-street parking denoted with "parking:lane:\<side\>" keys.
#'
#' This is done by converting the ways/lines to a `dodgr` network, and
#' matching vertices.
#' @noRd
process_onstreet_lanes <- function (dat_l) {

    # Column names retain the colons of the OSM keys, as returned by
    # `osmdata::osmdata_sf()`, like "parking:lane:both".
    keep_cols <- grep ("^parking:lane", names (dat_l$osm_lines), value = TRUE)
    net <- dodgr::weight_streetnet (
        dat_l$osm_lines,
        wt_profile = 1,
        keep_cols = keep_cols
    )

    # `dodgr` represents each way as two directed edges, but the parking
    # capacity of a street segment is the same in both directions, so keep only
    # one edge for each physical segment of each way.
    seg_key <- paste (
        net$way_id,
        pmin (net$from_id, net$to_id),
        pmax (net$from_id, net$to_id)
    )
    net <- net [!duplicated (seg_key), , drop = FALSE]

    # All possible combinations of parking lane tags:
    cols <- expand.grid (
        c ("both", "left", "right"),
        c ("", "parallel", "diagonal", "perpendicular")
    )
    cols <- paste0 ("parking:lane:", paste0 (cols [, 1], ":", cols [, 2]))
    cols <- gsub (":$", "", cols)
    cols <- cols [which (cols %in% keep_cols)]

    # Use the following translations (in metres), mostly from
    # https://en.wikipedia.org/wiki/Parking_space
    sizes <- c (
        parallel = 6.5,
        perpendicular = 3,
        diagonal = 4.75 # (par + perp) / 2
    )

    # Tag values which explicitly denote no parking on that side:
    no_parking <- c ("no", "none", "no_parking", "no_stopping", "separate")

    # First need to process the tags which just end in "<side>", and have values
    # identifying parking direction:
    col_side <- grep ("(both|left|right)$", cols, value = TRUE)
    for (co in col_side) {
        vals <- net [[co]]
        size <- sizes [match (vals, names (sizes))]
        has_tag <- !is.na (vals) & !vals %in% no_parking

        # replace any other sizes ("yes", "marked", "lane", ...) with random
        # samples from the actual distribution of sizes, or with the size of
        # parallel parking where there are no such values.
        index_out <- which (has_tag & is.na (size))
        if (length (index_out) > 0L) {
            values <- size [!is.na (size)]
            if (length (values) == 0L) {
                values <- sizes [["parallel"]]
            }
            # (Do not use `sample (values)`, which samples from `1:values`
            # when `values` is a single number.)
            i <- sample.int (length (values), length (index_out), replace = TRUE)
            size [index_out] <- values [i]
        }

        # The number of spaces on each side, with "both" having two sides:
        n_sides <- ifelse (co == "parking:lane:both", 2L, 1L)
        net [[co]] <- 0L
        net [[co]] [has_tag] <-
            n_sides * as.integer (floor (net$d [has_tag] / size [has_tag]))
    }

    # Other columns are then the explicit "parking:lane:<side>:<direction>"
    # tags. Key values for these don't matter, as they're mostly just
    # descriptions like "on_street" or "half_on_kerb", except that "no"-type
    # values denote no parking.
    cols_dir <- cols [which (!cols %in% col_side)]
    for (co in cols_dir) {
        has_tag <- !is.na (net [[co]]) & !net [[co]] %in% no_parking
        direction <- gsub ("^.*:", "", co)
        this_size <- sizes [[direction]]
        n_sides <- ifelse (grepl ("both", co), 2L, 1L)
        net [[co]] <- 0L
        net [[co]] [has_tag] <-
            n_sides * as.integer (floor (net$d [has_tag] / this_size))
    }

    if (length (cols) > 0L) {
        net$street_parking <- apply (net [, cols, drop = FALSE], 1, sum)
    } else {
        net$street_parking <- 0L
    }

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

    cols <- c ("building", "height")

    if (!is.null (planet_file)) {

        if (!quiet) {
            cli::cli_h3 (cli::col_green ("Extracting data on buildings:"))
        }
        osm_files <- osmium_process (planet_file, bb, city_name, quiet = quiet)

        f_b <- grep ("building", osm_files, value = TRUE)
        dat_b <- osmextract::oe_read (
            f_b,
            layer = "multipolygons",
            extra_tags = cols,
            quiet = TRUE
        ) |> sf::st_cast ("POLYGON")
        dat_b <- list (osm_polygons = dat_b)

    } else {

        dat_b <- osmdata::opq (bb) |>
            osmdata::add_osm_feature (key = "building") |>
            osmdata::osmdata_sf (quiet = quiet)
    }

    process_building_data (dat_b)
}

#' Process raw OSM building data
#'
#' @param dat_b Object returned from `osmdata::osmdata_sf()` with buildings
#' (polygons and, optionally, multipolygons).
#' @return Centroids of all buildings of sufficient size, with areas (in m^2)
#' and volumes (in m^3), both as plain numeric values.
#' @noRd
process_building_data <- function (dat_b) {

    # suppress no visible binding notes:
    building <- NULL

    # `sf` methods for subsetting must be registered:
    requireNamespace ("sf", quietly = TRUE)

    cols <- c ("building", "height")

    get_polys <- function (p) {
        for (col in cols [which (!cols %in% names (p))]) {
            p [[col]] <- NA_character_
        }
        keep <- c (intersect ("osm_id", names (p)), cols, "geometry")
        p [, keep]
    }
    p <- get_polys (dat_b$osm_polygons)
    if (!is.null (dat_b$osm_multipolygons) &&
        nrow (dat_b$osm_multipolygons) > 0L) {
        mp <- suppressWarnings (
            sf::st_cast (get_polys (dat_b$osm_multipolygons), "POLYGON")
        )
        p <- dplyr::bind_rows (p, mp)
    }
    p <- p [which (sf::st_is_valid (p)), ]
    p$height <- parse_height (p$height)

    p <- dplyr::group_by (p, building) |>
        dplyr::mutate_at ("height", fill_na_with_mean) |>
        dplyr::ungroup ()
    # Plain numbers, so that `volume` is not labelled with the units of area:
    p$area <- as.numeric (sf::st_area (p)) # m^2
    p$volume <- p$area * p$height # m^3
    p <- sf::st_centroid (p)

    # Finally, exclude any buildings < 20 m ^ 2 or < 3 m high
    p <- p [which (p$area > 20 & p$height > 3), ]

    return (p)
}

#' Rescale distance-weighted means so that their mean is the overall mean per
#' site.
#'
#' The weighted means `x` calculated by `rcpp_weighted_dists()` are means over
#' sites (vertices with some parking, or some buildings), and so in the units
#' of one site, but are biased by the weights. They are rescaled here so that
#' their mean over all vertices is the overall mean per site, `total /
#' n_sites`. This was previously calculated as `x * (n_vertices / n_sites) *
#' total / (mean (x) * n_vertices)`, in which `n_vertices` cancels, so is not
#' needed here.
#'
#' @param x Weighted means at each vertex.
#' @param total Total capacity (spaces) or volume (m^3) of all sites.
#' @param n_sites Number of sites.
#' @noRd
rescale_to_site_mean <- function (x, total, n_sites) {

    x * total / (n_sites * mean (x, na.rm = TRUE))
}

#' Aggregate raw parking data to each vertex of a contracted graph.
#'
#' @param parking A list of two objects: `dat_p` containing vertices and
#' capacities of parking from OSM nodes and polygons; and `dat_l` containing a
#' reduced `dodgr` street network with numbers of onstreet parking spaces
#' calculated in `process_onstreet_lanes()`.
#' @noRd
aggregate_parking_data <- function (graph_c, city, parking,
                                    dlim = 5000, k = 1000) {

    parking_lanes <- parking$dat_l
    parking <- parking$dat_p

    # suppress no visible binding notes:
    osm_id <- x <- y <- id <- NULL

    cli::cli_alert_info (cli::col_blue ("Aggregating parking data"))
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
    cli::cli_alert_success (cli::col_green ("Aggregated parking data"))

    capacity <- parking [["capacity"]]
    to <- parking$osm_id

    attr (graph_c, "turn_penalty") <- 0
    to_from_indices <- to_from_index_with_tp (graph_c, from, to)

    n_p <- length (to_from_indices$to$index)
    n_p <- format (n_p, big.mark = ",")
    cli::cli_alert_info (cli::col_blue (
        "Weighting {n_p} parking locations by distances to all vertices"
    ))
    d <- rcpp_weighted_dists (
        graph_c,
        to_from_indices$vert_map,
        to_from_indices$from$index,
        to_from_indices$to$index,
        capacity,
        dlim = dlim,
        k = k
    )
    cli::cli_alert_success (cli::col_green (
        "Weighted parking by distances to all vertices"
    ))

    capacity <- d [, 2] / d [, 1]
    capacity <- rescale_to_site_mean (
        capacity,
        sum (parking$capacity, na.rm = TRUE),
        nrow (parking)
    )

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

    cli::cli_alert_info (cli::col_blue ("Aggregating building volumes"))
    buildings <- dplyr::group_by (buildings, osm_id) |>
        dplyr::summarise (
            x = x [1],
            y = y [1],
            volume = sum (volume)
        )
    cli::cli_alert_success (cli::col_green ("Aggregated building volumes"))

    volume <- buildings [["volume"]]
    to <- buildings$osm_id

    attr (graph_c, "turn_penalty") <- 0
    to_from_indices <- to_from_index_with_tp (graph_c, from, to)

    n_b <- length (to_from_indices$to$index)
    n_b <- format (n_b, big.mark = ",")
    cli::cli_alert_info (cli::col_blue (
        "Weighting {n_b} buildings by distances to all vertices"
    ))
    vol_wt <- rcpp_weighted_dists (
        graph_c,
        to_from_indices$vert_map,
        to_from_indices$from$index,
        to_from_indices$to$index,
        volume,
        dlim = dlim,
        k = k
    )
    cli::cli_alert_success (cli::col_green (
        "Weighted buildings by distances to all vertices"
    ))

    volume <- vol_wt [, 2] / vol_wt [, 1]
    volume <- rescale_to_site_mean (
        volume,
        sum (buildings$volume, na.rm = TRUE),
        nrow (buildings)
    )

    return (volume)
}
