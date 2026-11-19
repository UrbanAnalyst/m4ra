# Extract data for parking function from a local OSM file with \pkg{rosmium},
# which wraps the 'osmium' command-line tool, and does not need it to be
# installed.

#' Convert a bounding box to the numeric vector required by \pkg{rosmium}.
#'
#' Accepts the forms of bounding box also accepted by `osmdata::opq()`: a
#' numeric vector of `c (xmin, ymin, xmax, ymax)`; a 2 x 2 matrix as returned
#' from `osmdata::getbb()`, with rows "x" and "y" and columns "min" and "max";
#' or a bounding box from \pkg{sf}, which is a named numeric vector.
#'
#' `rosmium` (and 'osmium') do not check coordinates, so wrongly ordered or
#' out-of-range values (such as the "south, west, north, east" order used by
#' Overpass) give an empty extract without any error.
#'
#' @param bb Bounding box.
#' @return Numeric vector of `c (xmin, ymin, xmax, ymax)`.
#' @noRd
osmium_bbox <- function (bb) {

    msg <- "Bounding box must be numeric `c (xmin, ymin, xmax, ymax)`"

    if (is.matrix (bb) && identical (dim (bb), c (2L, 2L))) {
        rn <- rownames (bb)
        cn <- colnames (bb)
        if (!is.null (rn) && !is.null (cn) &&
            all (c ("x", "y") %in% rn) && all (c ("min", "max") %in% cn)) {
            bb <- c (bb ["x", "min"], bb ["y", "min"], bb ["x", "max"], bb ["y", "max"])
        } else {
            # Same as `getbb ()`, with column-wise order of elements:
            bb <- c (bb)
        }
    }
    if (!is.null (names (bb)) &&
        all (c ("xmin", "ymin", "xmax", "ymax") %in% names (bb))) {
        bb <- bb [c ("xmin", "ymin", "xmax", "ymax")]
    }

    if (!is.numeric (bb) || length (bb) != 4L || any (!is.finite (bb))) {
        stop (msg, call. = FALSE)
    }
    bb <- as.numeric (bb)
    if (bb [1] >= bb [3] || bb [2] >= bb [4]) {
        stop (msg, ", with xmin < xmax and ymin < ymax", call. = FALSE)
    }
    if (any (abs (bb [c (1, 3)]) > 180) || any (abs (bb [c (2, 4)]) > 90)) {
        stop (
            msg, ", with longitudes within +/-180 and latitudes within +/-90",
            call. = FALSE
        )
    }

    return (bb)
}

#' Stop if an output path is the same file as an input path.
#'
#' The original OSM file must never be overwritten, and \pkg{rosmium} does not
#' check for this.
#' @noRd
osmium_check_not_input <- function (output, input) {

    if (normalizePath (output, mustWork = FALSE) ==
        normalizePath (input, mustWork = FALSE)) {
        stop (
            "Refusing to overwrite input file '", input, "'",
            call. = FALSE
        )
    }
}

#' Extract data within a bounding box from a local OSM file.
#'
#' The extract is saved in the same directory as `osm_extract`, as
#' `<city_name>.osm.pbf`. If that file already exists, it is used as it is, and
#' `osm_extract` is never modified, including where `osm_extract` itself is that
#' file.
#'
#' The "smart" strategy is used, which retains complete multipolygon relations
#' (such as large buildings); otherwise reading them later fails with "way can
#' not be found".
#' @noRd
osmium_cut <- function (osm_extract, bb, city_name, quiet = FALSE) {

    if (!fs::file_exists (osm_extract)) {
        stop ("osm_extract '", osm_extract, "' not found.", call. = FALSE)
    }

    planet_dir <- dirname (osm_extract)
    f <- fs::path (planet_dir, paste0 (city_name, ".osm.pbf"))

    if (fs::file_exists (f)) {
        if (!quiet) {
            cli::cli_alert_info (cli::col_green (f, " already exists"))
        }
        return (f)
    }

    bb <- osmium_bbox (bb)

    if (!quiet) {
        cli::cli_alert_info (cli::col_blue (
            "Extracting data within bounding box from OSM file"
        ))
    }

    # Write to a temporary file, so that an interrupted extraction does not
    # leave an incomplete file which would later be re-used:
    f_tmp <- fs::path (planet_dir, paste0 (city_name, "-tmp.osm.pbf"))
    osmium_check_not_input (f, osm_extract)
    osmium_check_not_input (f_tmp, osm_extract)

    rosmium::osmium_extract (
        osm_extract,
        f_tmp,
        bbox = bb,
        strategy = "smart",
        overwrite = TRUE
    )

    # The extract is silently empty for coordinates which are wrongly ordered
    # or do not overlap with the data.
    data_bb <- rosmium::osmium_fileinfo (
        f_tmp,
        extended = TRUE,
        get = "data.bbox"
    )
    if (grepl ("undefined", data_bb)) {
        fs::file_delete (f_tmp)
        stop (
            "No data found in '", osm_extract, "' within bounding box [",
            paste0 (bb, collapse = ", "),
            "]; coordinates must be in the order (xmin, ymin, xmax, ymax)",
            call. = FALSE
        )
    }
    fs::file_move (f_tmp, f)

    if (!quiet) {
        cli::cli_alert_success (cli::col_green (
            "Extracted data within bounding box from OSM file  "
        ))
    }

    return (f)
}

#' Filter an OSM file by specified tags
#'
#' @param f Path to full '.osm.pbf' file
#' @param name Name used to name the output file, as `<f>-<name>.osm`. This is
#' also used to name the elements of the vector returned from
#' `osmium_process()`.
#' @param tags Character vector of tag expressions to use for filtering, as
#' used by `osmium tags-filter`.
#' @param quiet Loud or quiet?
#' @return Path to filtered '.osm' (XML) file. This format is used because it
#' can be read directly by `osmdata::osmdata_sf (doc = )`.
#' @noRd
osmium_filter_tags <- function (f, name, tags, quiet = FALSE) {

    if (!nzchar (f)) {
        stop ("f must be specified", call. = FALSE)
    }

    if (!quiet) {
        cli::cli_h3 (name)
    }
    fosm <- paste0 (gsub ("\\.osm\\.pbf$", "", f), "-", name, ".osm")

    if (fs::file_exists (fosm)) {
        if (!quiet) {
            cli::cli_alert_info (cli::col_green (fosm, " already exists"))
        }
        return (fosm)
    }

    osmium_check_not_input (fosm, f)
    f_tmp <- paste0 (gsub ("\\.osm\\.pbf$", "", f), "-", name, "-tmp.osm")
    rosmium::osmium_tags_filter (f, f_tmp, expressions = tags, overwrite = TRUE)
    fs::file_move (f_tmp, fosm)

    return (fosm)
}

#' Extract the data needed for parking analyses from a local OSM file.
#'
#' @return Named character vector of paths of the city extract (`city`), and
#' filtered '.osm' files with `parking`, `amenity`, `parking_lane` and
#' `building` data. Names are not the same as those of the 'uaengine' file
#' "<city>-parking.osm", to prevent that file being used here.
#' @noRd
osmium_process <- function (osm_extract, bb, city_name, quiet = FALSE) {

    f <- osmium_cut (osm_extract, bb, city_name, quiet)

    f_parking <- osmium_filter_tags (
        f,
        name = "parking_key",
        tags = "nwr/parking",
        quiet = quiet
    )

    tags <- c (
        "nwr/amenity=parking",
        "nwr/building=garage",
        "nwr/building=garages"
    )
    f_amenity <- osmium_filter_tags (f, "amenity", tags, quiet = quiet)

    tags <- c (
        "w/parking:lane:left",
        "w/parking:lane:right",
        "w/parking:lane:both"
    )
    f_parking_lane <- osmium_filter_tags (f, "parking_lane", tags, quiet = quiet)

    f_building <- osmium_filter_tags (f, "building", "nwr/building", quiet = quiet)

    c (
        city = as.character (f),
        parking = f_parking,
        amenity = f_amenity,
        parking_lane = f_parking_lane,
        building = f_building
    )
}
