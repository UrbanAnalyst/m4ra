test_that ("bounding boxes are converted to numeric vectors", {

    bb <- c (13.39, 52.49, 13.41, 52.51)
    expect_equal (m4ra:::osmium_bbox (bb), bb)

    # as from `osmdata::getbb ()`:
    m <- matrix (bb, nrow = 2L, dimnames = list (c ("x", "y"), c ("min", "max")))
    expect_equal (m4ra:::osmium_bbox (m), bb)
    # with rows in the other order, names must be used:
    m2 <- m [c ("y", "x"), ]
    expect_equal (m4ra:::osmium_bbox (m2), bb)

    # as from `sf`:
    bb_sf <- structure (
        c (xmin = bb [1], ymin = bb [2], xmax = bb [3], ymax = bb [4]),
        class = "bbox"
    )
    expect_equal (m4ra:::osmium_bbox (bb_sf), bb)
    # named in different order:
    expect_equal (m4ra:::osmium_bbox (rev (bb_sf)), bb)
})

test_that ("invalid bounding boxes are rejected", {

    expect_error (m4ra:::osmium_bbox (c (1, 2, 3)), "xmin, ymin, xmax, ymax")
    expect_error (m4ra:::osmium_bbox (c (1, 2, 3, NA)), "xmin, ymin, xmax, ymax")
    expect_error (m4ra:::osmium_bbox (letters [1:4]), "xmin, ymin, xmax, ymax")
    # (Wrongly ordered coordinates such as (ymin, xmin, ymax, xmax) are only
    # detected where out of range, or by empty extracts, tested below.)
    expect_error (m4ra:::osmium_bbox (c (13.39, 100, 13.41, 120)), "+/-90")
    expect_error (m4ra:::osmium_bbox (c (13.41, 52.49, 13.39, 52.51)), "xmin < xmax")
    expect_error (m4ra:::osmium_bbox (c (13.39, 52.51, 13.41, 52.49)), "ymin < ymax")
    expect_error (m4ra:::osmium_bbox (c (13.39, 52.49, 13.41, 95)), "+/-90")
})

test_that ("extracts contain only data within the bounding box", {

    dir <- withr::local_tempdir ()
    f_in <- make_test_extract (dir)
    size_in <- file.size (f_in)
    md5_in <- unname (tools::md5sum (f_in))

    f <- m4ra:::osmium_cut (f_in, test_bb (), "testcity", quiet = TRUE)

    expect_equal (basename (f), "testcity.osm.pbf")
    expect_equal (dirname (f), dirname (f_in))
    expect_true (file.exists (f))
    # no temporary file is left over:
    expect_false (file.exists (fs::path (dir, "testcity-tmp.osm.pbf")))

    info <- rosmium::osmium_fileinfo (f, extended = TRUE, get = "data.bbox")
    expect_match (info, "13\\.4")
    expect_false (grepl ("13\\.9", info))

    # The original is never modified:
    expect_equal (file.size (f_in), size_in)
    expect_equal (unname (tools::md5sum (f_in)), md5_in)

    # and an existing extract is used as it is:
    mtime <- file.mtime (f)
    Sys.sleep (1.1)
    f2 <- m4ra:::osmium_cut (f_in, test_bb (), "testcity", quiet = TRUE)
    expect_equal (f2, f)
    expect_equal (file.mtime (f), mtime)
})

test_that ("an input file which is the extract itself is not overwritten", {

    dir <- withr::local_tempdir ()
    f_in <- make_test_extract (dir, name = "testcity")
    md5_in <- unname (tools::md5sum (f_in))

    # Input is "<city_name>.osm.pbf", so is used directly as the extract:
    f <- m4ra:::osmium_cut (f_in, test_bb (), "testcity", quiet = TRUE)
    expect_equal (normalizePath (f), normalizePath (f_in))
    expect_equal (unname (tools::md5sum (f_in)), md5_in)

    # The guard against overwriting is also directly testable:
    expect_error (
        m4ra:::osmium_check_not_input (f_in, f_in),
        "Refusing to overwrite input file"
    )
    expect_silent (m4ra:::osmium_check_not_input (fs::path (dir, "other.osm"), f_in))
    # also for different spellings of the same path:
    expect_error (
        m4ra:::osmium_check_not_input (fs::path (dir, ".", "testcity.osm.pbf"), f_in),
        "Refusing to overwrite input file"
    )
})

test_that ("empty extracts are an error and no file is left behind", {

    dir <- withr::local_tempdir ()
    f_in <- make_test_extract (dir)

    # bounding box which does not overlap the data:
    expect_error (
        m4ra:::osmium_cut (f_in, c (1, 1, 2, 2), "testcity", quiet = TRUE),
        "No data found"
    )
    expect_false (file.exists (fs::path (dir, "testcity.osm.pbf")))
    expect_false (file.exists (fs::path (dir, "testcity-tmp.osm.pbf")))

    # Coordinates in the order (ymin, xmin, ymax, xmax), which are valid
    # coordinates, but not where the data are:
    expect_error (
        m4ra:::osmium_cut (f_in, test_bb () [c (2, 1, 4, 3)], "testcity", quiet = TRUE),
        "No data found"
    )
    expect_false (file.exists (fs::path (dir, "testcity.osm.pbf")))

    # missing input
    expect_error (
        m4ra:::osmium_cut (fs::path (dir, "nope.osm.pbf"), test_bb (), "testcity"),
        "not found"
    )
})

test_that ("data are filtered to files which osmdata can read", {

    skip_if_not_installed ("osmdata")

    dir <- withr::local_tempdir ()
    f_in <- make_test_extract (dir)
    md5_in <- unname (tools::md5sum (f_in))

    files <- m4ra:::osmium_process (f_in, test_bb (), "testcity", quiet = TRUE)

    expect_named (files, c ("city", "parking", "amenity", "parking_lane", "building"))
    expect_true (all (file.exists (files)))
    expect_equal (unname (tools::md5sum (f_in)), md5_in)
    # None of the output files may be the name of the 'uaengine' file:
    expect_false (any (grepl ("testcity-parking\\.osm$", files)))
    # No temporary files are left over:
    expect_length (list.files (dir, pattern = "tmp"), 0L)

    read <- function (f) osmdata::osmdata_sf (doc = f, quiet = TRUE)

    # 'here' has a parking node and a building; 'there' has been excluded.
    # The 'parking' key has parking:lane etc. but only 'amenity=parking' here,
    # so is empty:
    d <- read (files [["amenity"]])
    expect_equal (nrow (d$osm_points [which (d$osm_points$amenity == "parking"), ]), 1L)
    expect_equal (d$osm_points$osm_id [which (d$osm_points$amenity == "parking")], "1")

    d <- read (files [["building"]])
    expect_equal (nrow (d$osm_polygons), 1L)
    expect_equal (d$osm_polygons$building, "yes")

    # One parking lane within the box (the other is 'there'):
    lanes <- osmdata::osmdata_sf (doc = files [["parking_lane"]], quiet = TRUE)
    expect_equal (rownames (lanes$osm_lines), "102")

    # Existing files are used again, and are not modified:
    mtimes <- file.mtime (files)
    Sys.sleep (1.1)
    files2 <- m4ra:::osmium_process (f_in, test_bb (), "testcity", quiet = TRUE)
    expect_equal (files2, files)
    expect_equal (file.mtime (files), mtimes)
})

test_that ("data are read from a local OSM extract", {

    skip_if_not_installed ("osmdata")

    dir <- withr::local_tempdir ()
    f_in <- make_test_extract (dir)

    # The argument has been renamed:
    expect_true ("osm_extract" %in% names (formals (m4ra_parking)))
    expect_false ("planet_file" %in% names (formals (m4ra_parking)))
    expect_true ("osm_extract" %in% names (formals (m4ra_prepare_data)))
    expect_false ("planet_file" %in% names (formals (m4ra_prepare_data)))

    # `bb` is not needed once the extract exists, as when called from a
    # workflow with an existing extract:
    dat <- m4ra:::get_parking_data (test_bb (), f_in, "testcity", quiet = TRUE)
    expect_named (dat, c ("dat_p", "dat_l"))
    # one point, from the 'here' parking node
    expect_equal (nrow (dat$dat_p), 1L)
    expect_equal (dat$dat_p$capacity, 30)
    expect_equal (dat$dat_p$osm_id, "1")
    # one street in the box, counted once, with both sides:
    expect_equal (length (unique (dat$dat_l$way_id)), 1L)
    expect_equal (dat$dat_l$way_id [1], "102")
    d <- geodist::geodist (
        cbind (x = 13.4050, y = 52.5020), cbind (x = 13.4050, y = 52.5030),
        measure = "haversine"
    ) [1, 1]
    expect_equal (sum (dat$dat_l$street_parking), 2 * floor (d / 6.5))

    # `bb` is not needed once the extract exists, as for the call from the
    # 'workflow' package which does not give it:
    dat2 <- m4ra:::get_parking_data (osm_extract = f_in, city_name = "testcity", quiet = TRUE)
    expect_equal (dat2, dat)

    b <- m4ra:::get_building_data (test_bb (), f_in, "testcity", quiet = TRUE)
    expect_equal (nrow (b), 1L)
    expect_equal (b$height, 10)
    expect_equal (b$volume, b$area * 10)

    # The original file has not been modified, and other files are used again:
    expect_true (file.exists (fs::path (dir, "testcity.osm.pbf")))
    expect_equal (basename (f_in), "planet.osm.pbf")
})
