# Tests for parking calculations. Helper functions to write small OSM files
# are in 'helper-osm.R'.

test_that ("Overpass query for amenity=parking uses correct key", {

    stop_msg <- "stop after capturing features"
    captured <- new.env ()

    testthat::local_mocked_bindings (
        opq = function (bbox, ...) bbox,
        add_osm_feature = function (opq, ...) opq,
        add_osm_features = function (opq, features, ...) {
            captured$features <- features
            stop (stop_msg)
        },
        # (force the piped query, which is otherwise lazily never evaluated)
        osmdata_sf = function (q, ...) {
            force (q)
            list ()
        },
        .package = "osmdata"
    )
    bb <- c (13.39, 52.49, 13.41, 52.51)
    err <- tryCatch (
        m4ra:::get_parking_data (bb, city_name = "test", quiet = TRUE),
        error = function (e) conditionMessage (e)
    )
    expect_identical (err, stop_msg)
    expect_false (any (grepl ("amentiy", captured$features)))
    expect_true (any (grepl ("\"amenity\"=\"parking\"", captured$features,
        fixed = TRUE
    )))
})

test_that ("polygon node IDs are used to remove points", {

    # A square parking polygon (way 10, nodes 1-4) plus one tagged node (1)
    # which is a vertex of that polygon, and one separate tagged node (9).
    tags <- c (amenity = "parking", capacity = "50")
    nodes <- c (
        osm_node (1, 52.5000, 13.4000, tags),
        osm_node (2, 52.5000, 13.4010),
        osm_node (3, 52.5010, 13.4010),
        osm_node (4, 52.5010, 13.4000),
        osm_node (9, 52.5100, 13.4100, tags)
    )
    ways <- osm_way (10, c (1, 2, 3, 4, 1), tags)
    f <- write_test_osm (nodes, ways)
    dat <- osmdata::osmdata_sf (doc = f, quiet = TRUE)

    res <- m4ra:::combine_pts_and_polys (dat)

    # one centroid for the polygon, plus the one separate point, but not the
    # polygon's own vertex:
    expect_equal (nrow (res), 2L)
    expect_false ("1" %in% res$osm_id)
    expect_true ("9" %in% res$osm_id)
})

test_that ("capacity parsing", {

    cap <- m4ra:::parse_capacity (
        c ("15", "10-20", "10;20", "abc", NA, "1,000", "50 spaces", "")
    )
    expect_type (cap, "double")
    expect_equal (cap [1], 15)
    # ranges give their mean
    expect_equal (cap [2], 15)
    # multiple values are summed, not concatenated to 1020
    expect_equal (cap [3], 30)
    expect_true (is.na (cap [4]))
    expect_true (is.na (cap [5]))
    expect_equal (cap [6], 1000)
    expect_equal (cap [7], 50)
    expect_true (is.na (cap [8]))
    expect_length (m4ra:::parse_capacity (character (0)), 0L)
})

test_that ("height parsing", {

    h <- m4ra:::parse_height (
        c ("10", "12.5", "12,5", "10 m", "10;12", "30 ft", "30 feet", "30'", NA, "tall", "")
    )
    expect_type (h, "double")
    expect_equal (h [1:5], c (10, 12.5, 12.5, 10, 10))
    expect_equal (h [6:8], rep (30 * 0.3048, 3))
    expect_true (all (is.na (h [9:11])))
    expect_length (m4ra:::parse_height (character (0)), 0L)
})

test_that ("NA values are filled with group means, but not with NaN", {

    expect_equal (m4ra:::fill_na_with_mean (c (1, 3, NA)), c (1, 3, 2))
    # all NA stays NA, and not NaN
    x <- m4ra:::fill_na_with_mean (c (NA_real_, NA_real_))
    expect_true (all (is.na (x) & !is.nan (x)))
})

test_that ("building data are processed to centroids with volumes", {

    # Three buildings: A with height of 10m; B without height, which should take
    # the mean for its "building" value; and C, which is too small.
    sq <- function (id0, lat0, lon0, size) {
        nodes <- c (
            osm_node (id0, lat0, lon0),
            osm_node (id0 + 1, lat0, lon0 + size),
            osm_node (id0 + 2, lat0 + size, lon0 + size),
            osm_node (id0 + 3, lat0 + size, lon0)
        )
        list (nodes = nodes, nds = c (id0 + 0:3, id0))
    }
    a <- sq (1, 52.50, 13.40, 0.001)
    b <- sq (11, 52.51, 13.40, 0.001)
    cc <- sq (21, 52.52, 13.40, 0.00005)
    ways <- c (
        osm_way (100, a$nds, c (building = "yes", height = "10 m")),
        osm_way (101, b$nds, c (building = "yes")),
        osm_way (102, cc$nds, c (building = "yes", height = "10"))
    )
    f <- write_test_osm (c (a$nodes, b$nodes, cc$nodes), ways)
    dat_b <- osmdata::osmdata_sf (doc = f, quiet = TRUE)

    res <- m4ra:::process_building_data (dat_b)

    expect_equal (nrow (res), 2L)
    expect_equal (res$height, c (10, 10))
    expect_equal (as.numeric (res$volume), as.numeric (res$area) * 10)
    expect_equal (as.numeric (res$area) [1], as.numeric (res$area) [2], tolerance = 0.02)
})

test_that ("on-street parking.lane.both counts are doubled, not halved", {

    nodes <- c (
        osm_node (1, 52.5000, 13.4000),
        osm_node (2, 52.5000, 13.4015),
        osm_node (3, 52.5010, 13.4015)
    )
    # (A second way with a different lane tag is included so that there are
    # two lane columns; a single column is tested separately below.)
    ways <- c (
        osm_way (10, 1:2, c (
            highway = "residential",
            "parking:lane:both" = "parallel"
        )),
        osm_way (11, 2:3, c (
            highway = "residential",
            "parking:lane:left" = "parallel"
        ))
    )
    net <- suppressMessages (
        m4ra:::process_onstreet_lanes (onstreet_input (ways, nodes))
    )
    net <- net [net$way_id == 10, ]

    d <- geodist::geodist (
        cbind (x = 13.4000, y = 52.5000),
        cbind (x = 13.4015, y = 52.5000),
        measure = "haversine"
    ) [1, 1]
    # parallel spaces are 6.5m, and "both" has a lane on each side:
    expected <- 2 * floor (d / 6.5)

    # The street is one physical segment, so is counted once even though
    # `dodgr` creates two directed edges:
    expect_equal (sum (net$street_parking), expected)
})

test_that ("on-street parking is not double-counted for directed edges", {

    nodes <- c (
        osm_node (1, 52.5000, 13.4000),
        osm_node (2, 52.5000, 13.4015),
        osm_node (3, 52.5010, 13.4015)
    )
    ways <- c (
        osm_way (10, 1:2, c (
            highway = "residential",
            "parking:lane:right" = "parallel"
        )),
        osm_way (11, 2:3, c (
            highway = "residential",
            "parking:lane:left" = "parallel"
        ))
    )
    net <- suppressMessages (
        m4ra:::process_onstreet_lanes (onstreet_input (ways, nodes))
    )
    net <- net [net$way_id == 10, ]
    d <- geodist::geodist (
        cbind (x = 13.4000, y = 52.5000),
        cbind (x = 13.4015, y = 52.5000),
        measure = "haversine"
    ) [1, 1]

    expect_equal (sum (net$street_parking), floor (d / 6.5))
})

test_that ("on-street parking works with a single lane-tag column", {

    nodes <- c (
        osm_node (1, 52.5000, 13.4000),
        osm_node (2, 52.5000, 13.4015)
    )
    ways <- osm_way (10, 1:2, c (
        highway = "residential",
        "parking:lane:right" = "parallel"
    ))
    net <- suppressMessages (
        m4ra:::process_onstreet_lanes (onstreet_input (ways, nodes))
    )
    expect_true (all (net$street_parking > 0))
})

test_that ("on-street parking with no standard lane values is finite", {

    # Only a non-standard value ("marked") for the lane tag: no sizes are
    # available to sample from.
    nodes <- c (
        osm_node (1, 52.5000, 13.4000),
        osm_node (2, 52.5000, 13.4015),
        osm_node (3, 52.5010, 13.4015)
    )
    ways <- c (
        osm_way (10, 1:2, c (
            highway = "residential",
            "parking:lane:both" = "parallel"
        )),
        osm_way (11, 2:3, c (
            highway = "residential",
            "parking:lane:left" = "marked"
        ))
    )
    net <- suppressMessages (suppressWarnings (
        m4ra:::process_onstreet_lanes (onstreet_input (ways, nodes))
    ))

    expect_false (anyNA (net$street_parking))
    expect_true (all (is.finite (net$street_parking)))
})

test_that ("on-street parking tagged as no parking has no spaces", {

    nodes <- c (
        osm_node (1, 52.5000, 13.4000),
        osm_node (2, 52.5000, 13.4015),
        osm_node (3, 52.5010, 13.4015),
        osm_node (4, 52.5010, 13.4000)
    )
    ways <- c (
        osm_way (10, 1:2, c (
            highway = "residential",
            "parking:lane:both" = "no"
        )),
        osm_way (11, 2:3, c (
            highway = "residential",
            "parking:lane:both" = "perpendicular"
        )),
        osm_way (12, 3:4, c (
            highway = "residential",
            "parking:lane:left:parallel" = "no_parking",
            "parking:lane:right:parallel" = "on_street"
        ))
    )
    net <- suppressMessages (
        m4ra:::process_onstreet_lanes (onstreet_input (ways, nodes))
    )
    expect_equal (net$street_parking [net$way_id == 10], 0)
    expect_true (net$street_parking [net$way_id == 11] > 0)

    # "no_parking" on one side gives no spaces there, while the other side counts:
    d <- geodist::geodist (
        cbind (x = 13.4015, y = 52.5010),
        cbind (x = 13.4000, y = 52.5010),
        measure = "haversine"
    ) [1, 1]
    expect_equal (net$street_parking [net$way_id == 12], floor (d / 6.5))
})

test_that ("on-street parking with a single standard lane size", {

    # Sampling from a single value must give that value, rather than a random
    # number from `1:value`.
    nodes <- c (
        osm_node (1, 52.5000, 13.4000),
        osm_node (2, 52.5000, 13.4015),
        osm_node (3, 52.5010, 13.4015)
    )
    ways <- c (
        osm_way (10, 1:2, c (
            highway = "residential",
            "parking:lane:left" = "perpendicular"
        )),
        osm_way (11, 2:3, c (
            highway = "residential",
            "parking:lane:left" = "marked"
        ))
    )
    d <- geodist::geodist (
        cbind (x = 13.4015, y = 52.5000),
        cbind (x = 13.4015, y = 52.5010),
        measure = "haversine"
    ) [1, 1]
    for (i in 1:10) {
        net <- suppressMessages (
            m4ra:::process_onstreet_lanes (onstreet_input (ways, nodes))
        )
        expect_equal (net$street_parking [net$way_id == 11], floor (d / 3))
    }
})

test_that ("weighted distance sums are weighted sums of exponential weights", {

    # A straight street with four vertices, 1 to 4, spaced at about 111m:
    nodes <- c (
        osm_node (1, 52.000, 13.000),
        osm_node (2, 52.001, 13.000),
        osm_node (3, 52.002, 13.000),
        osm_node (4, 52.003, 13.000)
    )
    ways <- osm_way (10, 1:4, c (highway = "residential"))
    f <- write_test_osm (nodes, ways)
    lines <- osmdata::osmdata_sf (doc = f, quiet = TRUE)$osm_lines
    net <- suppressMessages (dodgr::weight_streetnet (lines, wt_profile = "motorcar"))
    attr (net, "turn_penalty") <- 0
    net$.vx0 <- net$from_id
    net$.vx1 <- net$to_id
    seg <- net$d [net$from_id == "1" & net$to_id == "2"]

    ids <- as.character (1:4)
    k <- 1000
    weighted <- function (to, wts) {
        idx <- m4ra:::to_from_index_with_tp (net, ids, ids [to])
        m4ra:::rcpp_weighted_dists (
            net, idx$vert_map, idx$from$index, idx$to$index, wts,
            dlim = 5000, k = k
        )
    }

    to <- c (2, 4)
    wts <- c (10, 30)
    d <- weighted (to, wts)

    # Direct calculation: the first column is the sum of exp (-d / k), and the
    # second the sum of the same terms multiplied by the weights of the targets.
    dmat <- abs (outer (1:4, to, "-")) * seg
    expected1 <- rowSums (exp (-dmat / k))
    expected2 <- rowSums (exp (-dmat / k) * rep (wts, each = 4))
    expect_equal (d [, 1], expected1, tolerance = 1e-4)
    expect_equal (d [, 2], expected2, tolerance = 1e-4)

    # Results must not depend on the order of the targets
    d_rev <- weighted (rev (to), rev (wts))
    expect_equal (d_rev, d)
})

test_that ("add_parking_times uses start penalties for rows and end penalties for columns", {

    ids <- c ("a", "b", "c")
    parking <- data.frame (
        id = ids,
        penalty_start = c (10, 20, 30),
        penalty_end = c (100, 200, 300)
    )
    verts_car <- data.frame (id = ids)
    car_times <- matrix (1000, nrow = 2, ncol = 3)
    rownames (car_times) <- ids [1:2]
    colnames (car_times) <- ids

    cache_dir <- withr::local_tempdir ()
    hash <- substring (digest::digest (verts_car$id), 1, 6)
    saveRDS (parking, file.path (cache_dir, paste0 ("m4ra-test-parking-", hash, ".Rds")))
    testthat::local_mocked_bindings (m4ra_cache_dir = function (city = NULL) cache_dir)

    res <- m4ra:::add_parking_times (car_times, verts_car, "test")

    expected <- car_times +
        matrix (parking$penalty_start [1:2], nrow = 2, ncol = 3) +
        matrix (parking$penalty_end, nrow = 2, ncol = 3, byrow = TRUE)
    expect_equal (res, expected)
})

test_that ("add_parking_times returns times unchanged without a parking file, and errors with several", {

    ids <- c ("a", "b")
    verts_car <- data.frame (id = ids)
    car_times <- matrix (1000, nrow = 2, ncol = 2)
    rownames (car_times) <- colnames (car_times) <- ids

    # no parking files at all:
    cache_dir <- withr::local_tempdir ()
    testthat::local_mocked_bindings (m4ra_cache_dir = function (city = NULL) cache_dir)
    expect_identical (
        m4ra:::add_parking_times (car_times, verts_car, "test"),
        car_times
    )

    # two files matching the same hash should never happen, and is an error:
    hash <- substring (digest::digest (verts_car$id), 1, 6)
    parking <- data.frame (id = ids, penalty_start = 1, penalty_end = 1)
    saveRDS (parking, file.path (cache_dir, paste0 ("m4ra-test-parking-", hash, ".Rds")))
    saveRDS (parking, file.path (cache_dir, paste0 ("m4ra-test-parking-", hash, "-old.Rds")))
    expect_error (
        m4ra:::add_parking_times (car_times, verts_car, "test"),
        "Multiple parking files"
    )
})

# ---- Rescaling of aggregated parking and building values (T001-7) ----------

# A small weighted network of a square grid of streets, with a private cache
# directory. (Not contracted, because contracting such a small network leaves
# some vertices without coordinates.) The network is created from `osmdata_sc()`
# data, for
# which `dodgr` adds the ".vx0" and ".vx1" columns used by
# `rcpp_weighted_dists()`, as in the networks cached by \pkg{m4ra}.
rescaling_graph <- function (n = 6L) {

    d <- tempfile ()
    dir.create (d)
    Sys.setenv ("M4RA_CACHE_DIR" = d)

    ids <- matrix (seq_len (n^2), n, n)
    nodes <- unlist (lapply (seq_len (n), function (i) {
        lapply (seq_len (n), function (j) {
            osm_node (ids [i, j], 52 + i * 0.003, 5 + j * 0.004)
        })
    }))
    tags <- c (highway = "residential", oneway = "no")
    ways <- c (
        vapply (seq_len (n), function (i) osm_way (100L + i, ids [i, ], tags), ""),
        vapply (seq_len (n), function (j) osm_way (200L + j, ids [, j], tags), "")
    )
    f <- write_test_osm (nodes, ways)
    sc <- osmdata::osmdata_sc (doc = f, quiet = TRUE)
    net <- dodgr::weight_streetnet (sc, wt_profile = "motorcar")
    stopifnot (all (c (".vx0", ".vx1") %in% names (net)))

    return (net)
}

# Random points spread over the vertices of a graph, as a `data.frame` with
# coordinates in `x` and `y`
random_sites <- function (v, n, ..., seed = 1L) {

    set.seed (seed)
    sites <- v [sample (nrow (v), n), ]
    data.frame (x = sites$x, y = sites$y, ...)
}

test_that ("rescaling gives mean per site, whatever the number of vertices", {

    set.seed (1L)
    x <- runif (50, 1, 10)
    total <- 1234
    n_sites <- 40L

    res <- m4ra:::rescale_to_site_mean (x, total, n_sites)
    expect_equal (mean (res), total / n_sites)
    # Same distribution, only scaled:
    expect_equal (res / x, rep (res [1] / x [1], length (x)))

    # The previous formula, in which the number of vertices cancels:
    for (n_vert in c (60L, 600L, 6000L)) {
        res_old <- x * (n_vert / n_sites) * total /
            (mean (x) * n_vert)
        expect_equal (res, res_old)
    }

    # Estimated from a subset of values, with `NA` values ignored:
    x [3] <- NA
    res <- m4ra:::rescale_to_site_mean (x, total, n_sites)
    expect_equal (mean (res, na.rm = TRUE), total / n_sites)
})

test_that ("building volumes are plain numbers in cubic metres", {

    # Rectangle of 0.001 degrees of longitude (68.5 m at latitude 52) by 0.0005
    # degrees of latitude (55.5 m), so around 3,800 m^2, and 10 m high.
    nodes <- c (
        osm_node (1L, 52.0000, 5.000),
        osm_node (2L, 52.0000, 5.001),
        osm_node (3L, 52.0005, 5.001),
        osm_node (4L, 52.0005, 5.000)
    )
    ways <- osm_way (10L, c (1L, 2L, 3L, 4L, 1L), c (building = "yes", height = "10"))
    f <- write_test_osm (nodes, ways)
    dat_b <- osmdata::osmdata_sf (doc = f, quiet = TRUE)

    # The (single) building is below the minimum height if the unit conversion
    # is wrong:
    b <- m4ra:::process_building_data (dat_b)
    expect_equal (nrow (b), 1L)
    expect_false (inherits (b$volume, "units"))
    expect_false (inherits (b$area, "units"))
    expect_type (b$volume, "double")
    expect_equal (b$area, 3804, tolerance = 0.01)
    expect_equal (b$volume, 38040, tolerance = 0.01)
    expect_equal (b$volume, b$area * b$height)
})

test_that ("rescaled building volumes have the mean volume per site", {

    graph_c <- rescaling_graph ()
    v <- m4ra_vertices (graph_c, "test-grid")
    buildings <- random_sites (v, 20L, volume = seq (1000, 10000, length.out = 20L))

    suppressMessages (
        vol <- m4ra:::aggregate_building_data (graph_c, "test-grid", buildings)
    )
    # Sites are vertices with buildings, to which buildings are aggregated:
    idx <- dodgr::match_points_to_verts (v, buildings [, c ("x", "y")])
    vol_site <- tapply (buildings$volume, idx, sum)
    expect_equal (mean (vol), mean (vol_site))
})

test_that ("rescaled parking has the mean capacity per site", {

    graph_c <- rescaling_graph ()
    v <- m4ra_vertices (graph_c, "test-grid")
    capacity <- as.integer (seq (10, 200, length.out = 15L))
    dat_p <- random_sites (v, 15L, capacity = capacity, seed = 2L)
    dat_l <- data.frame (
        from_id = v$id [1], to_id = v$id [2],
        from_lon = v$x [1], from_lat = v$y [1],
        to_lon = v$x [2], to_lat = v$y [2],
        street_parking = 10L
    )
    parking <- list (dat_p = dat_p, dat_l = dat_l)

    suppressMessages (
        cap <- m4ra:::aggregate_parking_data (graph_c, "test-grid", parking)
    )

    # Sites are vertices with parking from points, or street lanes (half of
    # the parking at each end):
    idx <- dodgr::match_points_to_verts (v, dat_p [, c ("x", "y")])
    sites <- data.frame (
        id = c (v$id [idx], v$id [1:2]),
        capacity = c (capacity, 5L, 5L)
    )
    cap_site <- tapply (sites$capacity, sites$id, sum)
    expect_equal (mean (cap), mean (cap_site))
})

# Paths from `fs` always use forward slashes, but not those from `file.path ()`
# on Windows.
norm_path <- function (x) normalizePath (x, winslash = "/", mustWork = FALSE)

test_that ("cached parking files are found by the hash of vertex IDs", {

    ids <- c ("a", "b", "c")
    hash <- substring (digest::digest (ids), 1, 6)
    other_hash <- substring (digest::digest (c ("x", "y")), 1, 6)
    cache_dir <- withr::local_tempdir ()
    testthat::local_mocked_bindings (m4ra_cache_dir = function (city = NULL) cache_dir)
    save_file <- function (name) {
        f <- file.path (cache_dir, name)
        saveRDS (1, f)
        f
    }

    # none:
    expect_identical (m4ra:::find_parking_file ("test", ids), character (0L))
    # files for other networks, or without "parking", are not found:
    save_file (paste0 ("m4ra-test-parking-", other_hash, ".Rds"))
    save_file (paste0 ("m4ra-test-motorcar-graphc-", hash, ".Rds"))
    expect_identical (m4ra:::find_parking_file ("test", ids), character (0L))

    f <- save_file (paste0 ("m4ra-test-parking-", hash, ".Rds"))
    expect_equal (norm_path (m4ra:::find_parking_file ("test", ids)), norm_path (f))

    # More than one is an error:
    save_file (paste0 ("m4ra-test-parking-", hash, "-old.Rds"))
    expect_error (
        m4ra:::find_parking_file ("test", ids),
        "Multiple parking files"
    )
})

test_that ("the exported function finds the parking file for a city", {

    ids <- c ("a", "b", "c")
    hash <- substring (digest::digest (ids), 1, 6)
    cache_dir <- withr::local_tempdir ()
    testthat::local_mocked_bindings (
        m4ra_cache_dir = function (city = NULL) cache_dir,
        m4ra_load_cached_network = function (city, mode, contracted) {
            stopifnot (mode == "motorcar", contracted)
            "graph"
        },
        m4ra_vertices = function (graph, city) data.frame (id = ids)
    )
    expect_identical (m4ra_parking_file ("test"), character (0L))
    f <- file.path (cache_dir, paste0 ("m4ra-test-parking-", hash, ".Rds"))
    saveRDS (1, f)
    expect_equal (norm_path (m4ra_parking_file ("test")), norm_path (f))
})
