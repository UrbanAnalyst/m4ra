
#' Path to user cache directory
#' @noRd
m4ra_cache_dir <- function () {

    # use envvar is set, so tempdir can be used on CRAN checks
    cache_dir <- Sys.getenv ("M4RA_CACHE_DIR")

    if (cache_dir == "") {

        cache_dir <- fs::path_expand (fs::path (
            rappdirs::user_cache_dir (),
            "R",
            "m4ra"
        ))

        if (!dir.exists (cache_dir)) {
            dir.create (cache_dir, recursive = TRUE)
        }
    }

    return (cache_dir)
}

#' Return a unique hash for the input network.
#'
#' This can be used to identify cached versions of a given network, in
#' particular to determine whether or not weighted versions of a given network
#' have been previously cached.
#'
#' @inheritParams m4ra
#' @return Single character value with unique hash of given network.
#' @family cache
#' @export
m4ra_network_hash <- function (net) {

    checkmate::assert_class (net, "osmdata_sc")

    full_hash <- digest::digest (net)
    hash <- substr (full_hash, 1L, 6L)

    return (hash)
}

#' Save a weighted street network in both full and contracted forms to local
#' cache.
#'
#' This uses \pkg{fst}, which strips all attributes, so they are saved
#' separately, and re-attached in `load_cached_network()`.
#'
#' @param net The full network to be cached.
#' @param city Name of city; used to name cached network files.
#' @family cache
#' @export
m4ra_cache_network <- function (net, city) {

    city <- tolower (city)

    # Full graph:
    a <- names (attributes (net))
    a <- a [which (!a %in% c ("names", "row.names", "col.names", "px"))]
    a <- lapply (a, function (i) attr (net, i))

    hash <- attr (net, "hash")
    fname <- paste0 ("m4ra-", city, "-attr-", substring (hash, 1, 6), ".Rds")
    fpath <- file.path (tempdir (), fname)

    saveRDS (a, fpath)

    fst::write_fst (net, gsub ("attr", "net", fpath))

    # Contracted graph:
    netc <- dodgr::dodgr_contract_graph (net)
    a <- names (attributes (netc))
    a <- a [which (!a %in% c ("names", "row.names", "col.names", "px"))]
    a <- lapply (a, function (i) attr (net, i))

    hashc <- attr (netc, "hashc")
    fname <- paste0 ("m4ra-", city, "-attrc-", substring (hashc, 1, 6), ".Rds")
    fpath <- file.path (tempdir (), fname)

    saveRDS (a, fpath)

    fst::write_fst (netc, gsub ("attr", "netc", fpath))

    # Edge map and junctions:
    flist <- list.files (tempdir (), pattern = hashc, full.names = TRUE)
    edge_map <- readRDS (grep ("edge\\_map", flist, value = TRUE))
    fst::write_fst (edge_map, gsub ("attr", "edge-map", fpath))
    junctions <- readRDS (grep ("junctions", flist, value = TRUE))
    if (length (junctions) > 0L) {
        fst::write_fst (junctions, gsub ("attr", "junctions", fpath))
    }
}

#' Load cached file for one city and mode
#'
#' @param city City for which file is to be loaded.
#' @param mode One of "foot", "bicycle", or "motorcar".
#' @param filename Can be used to specify particular filename to load.
#' @return Previously cached, weighted streetnet for specified city and mode.
#' @family cache
#' @export
m4ra_load_cached_network <- function (city = NULL, mode = "foot",
                                      filename = NULL) {

    if (is.null (filename)) {
        city <- gsub ("\\s+", "-", tolower (city))
        checkmate::assert_character (city, max.len = 1L)
        checkmate::assert_character (mode, max.len = 1L)

        mode <- match.arg (tolower (mode), c ("foot", "bicycle", "motorcar"))

        flist <- fs::dir_ls (m4ra_cache_dir (), regexp = city)
        f <- grep (mode, flist, value = TRUE)
        if (length (f) != 1L) {
            stop (
                "No single file found for [city, mode] = [",
                city,
                ", ",
                mode,
                "]"
            )
        }
        checkmate::assert_file_exists (f)
    } else {
        checkmate::assert_file_exists (filename)
        f <- filename
    }

    graph <- fst::read_fst (f)

    class (graph) <- c ("dodgr_streetnet_sc", class (graph))
    attr (graph, "wt_profile") <- mode
    attr (graph, "left_side") <- FALSE
    attr (graph, "turn_penalty") <- ifelse (mode == "motorcar", 1., 0.)

    return (graph)
}
