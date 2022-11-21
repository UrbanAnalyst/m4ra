
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

    full_hash <- attr (net, "hash")
    if (!is.null (full_hash)) {
        return (full_hash)
    }

    ids <- list (
        nodes = net$nodes$vertex_,
        obj = net$object$object_,
        edge = net$edge$edge_,
        vertex = net$vertex$vertex_,
        meta = net$meta
    )

    full_hash <- digest::digest (ids)
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
#' @param mode Mode of transport, as name of weighting profile used to weight
#' networks in \pkg{dodgr}; used to name cached network files.
#' @family cache
#' @export
m4ra_cache_network <- function (net, city, mode) {

    flist_out <- NULL

    cache_dir <- fs::path (m4ra_cache_dir (), city)
    city <- tolower (city)

    # Full graph:
    a <- names (attributes (net))
    a_nms <- a [which (!a %in% c ("names", "row.names", "col.names", "px"))]
    a <- lapply (a_nms, function (i) attr (net, i))
    names (a) <- a_nms

    hash <- attr (net, "hash")
    fname <- paste0 ("m4ra-", city, "-", mode, "-attr-",
        substring (hash, 1, 6), ".Rds")
    fpath <- file.path (cache_dir, fname)
    flist_out <- c (flist_out, fpath)

    saveRDS (a, fpath)

    fa <- gsub ("attr", "net", fpath)
    fst::write_fst (net, fa)
    flist_out <- c (flist_out, fa)

    # Contracted graph:
    netc <- dodgr::dodgr_contract_graph (net)
    a <- names (attributes (netc))
    a_nms <- a [which (!a %in% c ("names", "row.names", "col.names", "px"))]
    a <- lapply (a_nms, function (i) attr (netc, i))
    names (a) <- a_nms

    hashc <- a$hashc
    fname <- paste0 ("m4ra-", city, "-", mode, "-attrc-",
        substring (hashc, 1, 6), ".Rds")
    fpath <- file.path (cache_dir, fname)

    saveRDS (a, fpath)
    flist_out <- c (flist_out, fpath)

    fa <- gsub ("attrc", "netc", fpath)
    fst::write_fst (netc, fa)
    flist_out <- c (flist_out, fa)

    # Edge map and junctions:
    flist <- list.files (fs::path_temp (), pattern = hashc, full.names = TRUE)
    edge_map <- readRDS (grep ("edge\\_map", flist, value = TRUE))
    fe <- gsub ("attrc", "edge-map", fpath)
    fst::write_fst (edge_map, fe)
    flist_out <- c (flist_out, fe)
    junctions <- readRDS (grep ("junctions", flist, value = TRUE))
    if (length (junctions) > 0L) {
        fj <- gsub ("attrc", "junctions", fpath)
        fst::write_fst (junctions, fj)
        flist_out <- c (flist_out, fj)
    }

    return (flist_out)
}

#' Load cached file for one city and mode
#'
#' @param city City for which file is to be loaded.
#' @param mode One of "foot", "bicycle", or "motorcar".
#' @param contracted If `TRUE`, load the contracted version of the graph;
#' otherwise load the full graph.
#' @param filename Optional name of specific file to load.
#' @return Previously cached, weighted streetnet for specified city and mode.
#' @family cache
#' @export
m4ra_load_cached_network <- function (city = NULL, mode = "foot",
                                      contracted = TRUE, filename = NULL) {

    ptn <- paste0 ("\\-", ifelse (contracted, "netc", "net"), "\\-")

    if (!is.null (filename)) {

        checkmate::assert_file_exists (filename)
        dirs <- fs::path_split (filename) [[1]]
        city <- dirs [length (dirs) - 1L]
        f <- filename

        flist <- fs::dir_ls (fs::path (m4ra_cache_dir (), city), regexp = mode)

    } else {

        city <- gsub ("\\s+", "-", tolower (city))
        checkmate::assert_character (city, max.len = 1L)
        checkmate::assert_character (mode, max.len = 1L)

        mode <- match.arg (tolower (mode), c ("foot", "bicycle", "motorcar"))

        flist <- fs::dir_ls (fs::path (m4ra_cache_dir (), city), regexp = mode)

        f <- grep (ptn, flist, value = TRUE)

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

    }

    graph <- fst::read_fst (f)

    ptn <- gsub ("net", "attr", ptn) # retains 'attrc' for contracted
    a <- readRDS (grep (ptn, flist, value = TRUE))
    for (i in seq_along (a)) {
        attr (graph, names (a) [i]) <- a [[i]]
    }

    return (graph)
}

m_readRDS <- memoise::memoise (function (path) readRDS (path))
