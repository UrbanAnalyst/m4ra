# Extrac data for parking function from a local planet file with osmium

osmium_cut <- function (planet_file, bb, city_name, quiet = FALSE) {

    if (!nzchar (Sys.which ("osmium"))) {
        stop ("osmium must be installed to use local pbf/bz2 files.",
            call. = FALSE)
    }
    if (!file.exists (planet_file)) {
        stop ("planet_file '", planet_file, "' not found.",
            call. = FALSE)
    }

    planet_dir <- dirname (planet_file)
    f <- file.path (planet_dir, paste0 (city_name, ".osm.pbf"))

    cmd <- paste ("osmium extract -b", paste0 (bb, collapse = ","),
        planet_file, "-o", f)

    if (file.exists (f)) {
        if (!quiet) {
            cli::cli_alert_info (cli::col_green (f, " already exists"))
        }
        return (f)
    }

    if (!quiet) {
        cli::cli_alert_info (cli::col_blue (
            "Extracting data within bounding box from planet file"))
    }

    system (cmd)

    if (!quiet) {
        cli::cli_alert_success (cli::col_green (
            "Extracted data within bounding box from planet file  "))
    }

    return (f)
}

osmium_tags_filter <- function (f, tags, quiet = FALSE) {

    if (!nzchar (f)) {
        stop ("f must be specified", call. = FALSE)
    }

    tag1 <- gsub ("\\=.*$", "", tags [1])
    if (grepl ("lane", tag1)) {
        tag1 <- "parking_lane"
    }
    tags <- paste0 (tags, collapse = " ")
    if (!quiet) {
        cli::cli_h3 (tag1)
    }
    f_tag <- paste0 (gsub ("\\.osm\\.pbf$", "", f), "-", tag1, ".osm.pbf")
    fosm <- tools::file_path_sans_ext (f_tag) # removes ".pbf"

    if (file.exists (fosm)) {
        if (!quiet) {
            cli::cli_alert_info (cli::col_green (fosm, " already exists"))
        }
        return (fosm)
    }

    cmd <- paste ("osmium tags-filter --no-progress", f, tags, "-o", f_tag)
    system (cmd)

    # Then 'osmium cat' to convert to .osm/.xml format:
    cmd <- paste ("osmium cat --no-progress", f_tag, "-o", fosm)
    system (cmd)

    file.remove (f_tag)

    return (fosm)
}

osmium_process <- function (planet_file, bb, city_name, quiet = FALSE) {

    f <- osmium_cut (planet_file, bb, city_name, quiet)

    f_parking <- osmium_tags_filter (f, tags = "parking", quiet = quiet)

    tags <- c (
        "amenity=parking",
        "building=garage",
        "building=garages"
    )
    f_amenity <- osmium_tags_filter (f, tags, quiet = quiet)

    tags <- c (
        "parking:lane:left",
        "parking:lane:right",
        "parking:lane:both"
    )
    f_parking_lane <- osmium_tags_filter (f, tags, quiet = quiet)

    f_building <- osmium_tags_filter (f, "building", quiet = quiet)

    c (f, f_parking, f_amenity, f_parking_lane, f_building)
}
