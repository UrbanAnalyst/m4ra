# Functions to extract attributes and coordinates from objects returned by
# `osmdata::osmdata_sf()`, and to calculate areas and centroids of polygons,
# without using any functions from the \pkg{sf} package. Objects of class "sf"
# are just data.frames with a list-column of geometries, in which points are
# numeric vectors, and polygons are lists of numeric matrices, one for each
# ring (with the outer ring first, and any holes following). Row names of the
# matrices of polygons are the IDs of the OSM nodes.

#' Extract attributes and geometries from an "sf" object from \pkg{osmdata}.
#'
#' @param x An "sf" object from `osmdata::osmdata_sf()`, or `NULL`.
#' @param cols Names of columns to extract. Any which do not exist are filled
#' with `NA`.
#' @return A list of `data`, a `data.frame` with an `osm_id` column, and all
#' `cols` (as character values), and `geometry`, a list of the geometries.
#' @noRd
osm_sf_parts <- function (x, cols = NULL) {

    if (is.null (x) || is.null (x$geometry)) {
        dat <- data.frame (osm_id = character (0L))
        for (col in cols) {
            dat [[col]] <- character (0L)
        }
        return (list (data = dat, geometry = list ()))
    }

    # Work with plain lists, so that no methods for "sf" objects are used:
    n <- length (x$geometry)
    ids <- attr (x, "row.names")
    if (!is.null (x$osm_id)) {
        ids <- x$osm_id
    }
    dat <- data.frame (osm_id = as.character (ids), stringsAsFactors = FALSE)
    for (col in cols) {
        if (is.null (x [[col]])) {
            dat [[col]] <- rep (NA_character_, n)
        } else {
            dat [[col]] <- as.character (x [[col]])
        }
    }

    list (data = dat, geometry = unclass (x$geometry))
}

#' Matrix of coordinates of a list of points, as returned from
#' `osm_sf_parts()`.
#' @return A `data.frame` of `x` and `y` columns.
#' @noRd
point_xy <- function (geometry) {

    if (length (geometry) == 0L) {
        return (data.frame (x = numeric (0L), y = numeric (0L)))
    }
    xy <- matrix (unlist (lapply (geometry, unclass)), ncol = 2L, byrow = TRUE)
    data.frame (x = xy [, 1], y = xy [, 2])
}

#' Expand a list of multipolygons into a list of polygons.
#'
#' @param geometry List of multipolygons, each of which is a list of polygons.
#' @return A list of `polygons`, each of which is a list of rings, and `index`,
#' the index of the multipolygon from which each polygon came.
#' @noRd
multipolygon_parts <- function (geometry) {

    members <- lapply (geometry, unclass)
    list (
        polygons = do.call (c, members),
        index = rep (seq_along (members), lengths (members))
    )
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

#' Areas, centroids and validity of polygons, calculated without \pkg{sf}.
#'
#' Coordinates of each ring are first converted to metres, relative to the first
#' vertex of that ring. The east-west distance of each vertex is the haversine
#' distance (from \pkg{geodist}) along the latitude of that vertex, and the
#' north-south distance is the haversine distance along the meridian. These are
#' local, planar coordinates, and are accurate for objects the size of
#' buildings, or parking lots, rather than for polygons spanning large parts of
#' a country. Areas are calculated from these with the shoelace formula.
#' Rings of each polygon which are nested within an odd number of the other
#' rings are holes, and their areas are subtracted. The others are outer rings,
#' and their areas are added.
#'
#' Centroids are the area-weighted centroids of the rings (with holes counting
#' negatively), calculated from the longitudes and latitudes. They are not
#' changed by the linear scaling between degrees and metres.
#'
#' Haversine distances from \pkg{geodist} use the equatorial radius of the
#' Earth, so that areas are around 0.2% larger than those calculated on a sphere
#' of the same area as the Earth (as by \pkg{sf} with \pkg{s2}), at the
#' latitudes of Europe.
#'
#' Polygons are valid if their outer ring is closed, has at least four points,
#' and a non-zero area. Unlike validity as tested for by \pkg{sf}, this does not
#' test for rings which cross themselves.
#'
#' @param polygons List of polygons, each of which is a list of rings, each of
#' which is a numeric matrix of longitudes and latitudes, with the outer ring
#' first.
#' @return A `data.frame` with one row for each polygon, with columns of `valid`,
#' `area` (in square metres), and the centroid in `x` (longitude) and `y`
#' (latitude).
#' @noRd
polygon_stats <- function (polygons) {

    np <- length (polygons)
    if (np == 0L) {
        return (data.frame (
            valid = logical (0L), area = numeric (0L),
            x = numeric (0L), y = numeric (0L)
        ))
    }

    n_rings <- lengths (polygons)
    rings <- do.call (c, lapply (polygons, unclass))
    n <- vapply (rings, nrow, integer (1L))
    ring_poly <- rep (seq_len (np), n_rings)
    ring_outer <- !duplicated (ring_poly)

    lon <- unlist (lapply (rings, function (i) i [, 1]))
    lat <- unlist (lapply (rings, function (i) i [, 2]))
    nr <- length (rings)
    rid <- rep (seq_len (nr), n)
    first <- cumsum (c (1L, n)) [seq_len (nr)]
    last <- cumsum (n)

    ring_closed <- n >= 4L &
        lon [first] == lon [last] & lat [first] == lat [last]

    # Local coordinates in metres relative to the first vertex of each ring
    lon0 <- lon [first] [rid]
    lat0 <- lat [first] [rid]
    metres_east <- geodist::geodist_vec (
        lon0, lat, lon, lat,
        paired = TRUE, measure = "haversine"
    ) * sign (lon - lon0)
    metres_north <- geodist::geodist_vec (
        lon0, lat0, lon0, lat,
        paired = TRUE, measure = "haversine"
    ) * sign (lat - lat0)

    # Cross-products of successive vertices of each ring, in metres and degrees
    N <- length (lon)
    same <- rid [-N] == rid [-1]
    i1 <- which (same)
    i2 <- i1 + 1L
    cross_m <- metres_east [i1] * metres_north [i2] -
        metres_east [i2] * metres_north [i1]
    dlon <- lon - lon0
    dlat <- lat - lat0
    cross_d <- dlon [i1] * dlat [i2] - dlon [i2] * dlat [i1]

    sum_by_ring <- function (x) {
        res <- numeric (nr)
        s <- rowsum (x, rid [i1])
        res [as.integer (rownames (s))] <- s [, 1]
        res
    }
    ring_area <- abs (0.5 * sum_by_ring (cross_m))
    ring_area [!ring_closed] <- 0

    # Rings are holes if nested within an odd number of other rings of the same
    # polygon, and are otherwise outer rings (which `osmdata` can return as
    # further rings of the same polygon). Only rings with larger areas can
    # contain a ring.
    ring_hole <- rep (FALSE, nr)
    multi <- which (n_rings > 1L)
    for (p in multi) {
        r <- which (ring_poly == p & ring_closed)
        for (i in r [-1L]) {
            container <- r [which (ring_area [r] > ring_area [i])]
            depth <- sum (vapply (container, function (j) {
                mean (points_in_ring (
                    lon [rid == i], lat [rid == i],
                    lon [rid == j], lat [rid == j]
                )) > 0.5
            }, logical (1L)))
            ring_hole [i] <- depth %% 2L == 1L
        }
    }
    ring_sign <- ifelse (ring_hole, -1, 1)

    area <- as.numeric (tapply (ring_sign * ring_area, ring_poly, sum))

    # Centroids from the polygon formula, using degrees relative to the first
    # vertex of each ring, weighted by the (signed) areas of rings in degrees.
    # Zero-area rings use mean coordinates.
    a2 <- sum_by_ring (cross_d)
    cx <- sum_by_ring ((dlon [i1] + dlon [i2]) * cross_d)
    cy <- sum_by_ring ((dlat [i1] + dlat [i2]) * cross_d)
    ok <- a2 != 0
    ring_x <- ifelse (ok, cx / (3 * a2), NA_real_) + lon [first]
    ring_y <- ifelse (ok, cy / (3 * a2), NA_real_) + lat [first]
    ring_x [!ok] <- tapply (lon, rid, mean) [!ok]
    ring_y [!ok] <- tapply (lat, rid, mean) [!ok]
    w <- ring_sign * abs (a2) * ring_closed
    wsum <- tapply (w, ring_poly, sum)
    x <- tapply (w * ring_x, ring_poly, sum) / wsum
    y <- tapply (w * ring_y, ring_poly, sum) / wsum
    # (Outer rings only, if all areas cancel)
    bad <- !is.finite (x) | !is.finite (y)
    x [bad] <- ring_x [ring_outer] [bad]
    y [bad] <- ring_y [ring_outer] [bad]

    data.frame (
        valid = ring_closed [ring_outer] & area > 0,
        area = area,
        x = as.numeric (x),
        y = as.numeric (y)
    )
}

#' Test whether points are inside a ring, by counting crossings of a ray
#' extending to the east. Points exactly on the boundary may be either inside
#' or outside.
#'
#' @param px,py Coordinates of points to test.
#' @param rx,ry Coordinates of ring, with the last point the same as the first.
#' @return Logical vector of the same length as `px`.
#' @noRd
points_in_ring <- function (px, py, rx, ry) {

    n <- length (rx) - 1L
    x1 <- rx [seq_len (n)]
    y1 <- ry [seq_len (n)]
    x2 <- rx [seq_len (n) + 1L]
    y2 <- ry [seq_len (n) + 1L]

    vapply (seq_along (px), function (i) {
        straddles <- (y1 > py [i]) != (y2 > py [i])
        x_cross <- x1 + (py [i] - y1) * (x2 - x1) / (y2 - y1)
        sum (straddles & px [i] < x_cross) %% 2L == 1L
    }, logical (1L))
}
