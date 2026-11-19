# Rectangular ring of longitudes (`x0` to `x1`) and latitudes (`y0` to `y1`),
# as a matrix with node IDs as row names, like `osmdata_sf()` polygons.
ring <- function (x0, y0, x1, y1, id0 = 1L, clockwise = FALSE) {

    m <- cbind (
        lon = c (x0, x1, x1, x0, x0),
        lat = c (y0, y0, y1, y1, y0)
    )
    if (clockwise) {
        m <- m [c (1, 4, 3, 2, 1), ]
    }
    rownames (m) <- as.character (c (id0 + 0:3, id0))
    m
}

# Distances in metres, as used within `polygon_stats ()`:
metres <- function (x0, y0, x1, y1) {
    geodist::geodist (
        cbind (x = x0, y = y0), cbind (x = x1, y = y1),
        measure = "haversine"
    ) [1, 1]
}

test_that ("polygon areas and centroids of rectangles", {

    x0 <- 5.000
    x1 <- 5.001
    y0 <- 52.000
    y1 <- 52.0005
    expected <- metres (x0, y0, x1, y0) * metres (x0, y0, x0, y1)

    # in either direction around the ring:
    for (cw in c (FALSE, TRUE)) {
        st <- m4ra:::polygon_stats (list (list (ring (x0, y0, x1, y1, clockwise = cw))))
        expect_equal (nrow (st), 1L)
        expect_true (st$valid)
        expect_equal (st$area, expected, tolerance = 1e-4)
        expect_equal (st$x, mean (c (x0, x1)))
        expect_equal (st$y, mean (c (y0, y1)))
    }

    # Area is about 3,800 m^2, as calculated with `sf` (using a sphere, so with
    # a difference of 0.2%).
    expect_equal (expected, 3808, tolerance = 0.01)
})

test_that ("polygon areas and centroids of triangles", {

    x0 <- 5.000
    x1 <- 5.002
    y0 <- 52.000
    y1 <- 52.001
    tri <- cbind (
        lon = c (x0, x1, x0, x0),
        lat = c (y0, y0, y1, y0)
    )
    rownames (tri) <- c ("1", "2", "3", "1")
    st <- m4ra:::polygon_stats (list (list (tri)))
    expected <- 0.5 * metres (x0, y0, x1, y0) * metres (x0, y0, x0, y1)
    expect_true (st$valid)
    expect_equal (st$area, expected, tolerance = 1e-4)
    # centroid of a triangle is the mean of its vertices:
    expect_equal (st$x, mean (c (x0, x1, x0)))
    expect_equal (st$y, mean (c (y0, y0, y1)))
})

test_that ("polygons with holes, and several outer rings", {

    outer <- ring (5.000, 52.000, 5.002, 52.001)
    hole <- ring (5.0005, 52.0002, 5.0015, 52.0008, id0 = 11L)
    a_outer <- m4ra:::polygon_stats (list (list (outer)))$area
    a_hole <- m4ra:::polygon_stats (list (list (hole)))$area

    # Hole is subtracted, whichever direction it is in:
    st <- m4ra:::polygon_stats (list (list (outer, hole)))
    expect_equal (st$area, a_outer - a_hole)
    hole_cw <- ring (5.0005, 52.0002, 5.0015, 52.0008, id0 = 11L, clockwise = TRUE)
    st <- m4ra:::polygon_stats (list (list (outer, hole_cw)))
    expect_equal (st$area, a_outer - a_hole)
    # Hole in the middle does not change the centroid:
    expect_equal (st$x, 5.001)
    expect_equal (st$y, 52.0005)

    # An off-centre hole moves the centroid away from it:
    hole2 <- ring (5.0001, 52.0002, 5.0009, 52.0008, id0 = 11L)
    st <- m4ra:::polygon_stats (list (list (outer, hole2)))
    expect_true (st$x > 5.001)
    expect_equal (st$y, 52.0005)

    # An island inside the hole counts as an outer ring:
    island <- ring (5.0008, 52.0004, 5.0012, 52.0006, id0 = 21L)
    a_island <- m4ra:::polygon_stats (list (list (island)))$area
    st <- m4ra:::polygon_stats (list (list (outer, hole, island)))
    expect_equal (st$area, a_outer - a_hole + a_island)

    # A second ring which is not within the first is another outer ring:
    other <- ring (5.003, 52.000, 5.004, 52.001, id0 = 31L)
    a_other <- m4ra:::polygon_stats (list (list (other)))$area
    st <- m4ra:::polygon_stats (list (list (outer, other)))
    expect_true (st$valid)
    expect_equal (st$area, a_outer + a_other)
    # with centroid between the two, nearer to the bigger one:
    expect_true (st$x > 5.001 && st$x < 5.0035)
})

test_that ("invalid polygons are identified", {

    good <- ring (5.000, 52.000, 5.001, 52.001)
    # Not closed:
    open_ring <- good [1:4, ]
    # Too few points:
    two <- good [c (1, 2, 1), ]
    # No area:
    flat <- cbind (lon = c (5, 5.001, 5.002, 5), lat = rep (52, 4))
    rownames (flat) <- c ("1", "2", "3", "1")

    st <- m4ra:::polygon_stats (list (
        list (good), list (open_ring), list (two), list (flat)
    ))
    expect_equal (st$valid, c (TRUE, FALSE, FALSE, FALSE))
    expect_equal (nrow (m4ra:::polygon_stats (list ())), 0L)
})

test_that ("many polygons are processed together", {

    # Same results as for individual polygons
    set.seed (1L)
    x0 <- runif (20, 5, 5.1)
    y0 <- runif (20, 52, 52.1)
    polys <- lapply (seq_along (x0), function (i) {
        list (ring (x0 [i], y0 [i], x0 [i] + 0.001 * i, y0 [i] + 0.0005 * i))
    })
    st <- m4ra:::polygon_stats (polys)
    for (i in c (1, 7, 20)) {
        st_i <- m4ra:::polygon_stats (polys [i])
        expect_equal (st [i, ], st_i, ignore_attr = TRUE)
    }
    expect_true (all (st$valid))
    # areas increase with the square of the size:
    expect_true (cor (st$area, seq_along (x0)^2) > 0.99)
})

test_that ("points in rings", {

    r <- ring (0, 0, 2, 1)
    expect_equal (
        m4ra:::points_in_ring (c (1, 3, 1, -1), c (0.5, 0.5, 2, 0.5), r [, 1], r [, 2]),
        c (TRUE, FALSE, FALSE, FALSE)
    )
})

test_that ("attributes and coordinates are extracted from osmdata objects", {

    tags <- c (amenity = "parking", capacity = "30")
    nodes <- c (
        osm_node (1L, 52.000, 13.000, tags),
        osm_node (2L, 52.001, 13.002),
        osm_node (3L, 52.002, 13.002),
        osm_node (4L, 52.002, 13.000),
        osm_node (5L, 52.010, 13.010, c (amenity = "parking"))
    )
    ways <- osm_way (10L, c (2L, 3L, 4L, 2L), c (amenity = "parking", name = "x"))
    f <- write_test_osm (nodes, ways)
    dat <- osmdata::osmdata_sf (doc = f, quiet = TRUE)

    pts <- m4ra:::osm_sf_parts (dat$osm_points, c ("amenity", "capacity", "absent"))
    expect_named (pts, c ("data", "geometry"))
    expect_equal (pts$data$osm_id, c ("1", "2", "3", "4", "5"))
    expect_equal (pts$data$capacity, c ("30", NA, NA, NA, NA))
    expect_true (all (is.na (pts$data$absent)))
    expect_false (inherits (pts$data, "sf"))
    xy <- m4ra:::point_xy (pts$geometry)
    expect_equal (xy$x [1], 13.000)
    expect_equal (xy$y [5], 52.010)

    poly <- m4ra:::osm_sf_parts (dat$osm_polygons, "amenity")
    expect_equal (poly$data$osm_id, "10")
    expect_equal (m4ra:::polygon_node_ids (poly$geometry), c ("2", "3", "4"))

    # missing objects give empty results
    none <- m4ra:::osm_sf_parts (NULL, c ("a", "b"))
    expect_equal (nrow (none$data), 0L)
    expect_named (none$data, c ("osm_id", "a", "b"))
    expect_equal (nrow (m4ra:::point_xy (none$geometry)), 0L)
})

test_that ("multipolygons are split into polygons", {

    # A polygon is a list of rings, and a multipolygon is a list of polygons:
    p1 <- list (ring (5, 52, 5.001, 52.001))
    p2 <- list (ring (5.002, 52, 5.003, 52.001, id0 = 11L))
    p3 <- list (ring (5.004, 52, 5.005, 52.001, id0 = 21L))
    mp <- m4ra:::multipolygon_parts (list (list (p1, p2), list (p3)))
    expect_length (mp$polygons, 3L)
    expect_equal (mp$index, c (1L, 1L, 2L))
    expect_equal (mp$polygons [[3]] [[1]], p3 [[1]])
})
