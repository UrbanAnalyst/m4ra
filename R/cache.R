get_hash <- function (graph, contracted = FALSE, force = FALSE) {

    hash <- NULL
    if (!force) {
        hash <- attr (graph, ifelse (contracted, "hashc", "hash"))
    }

    if (is.null (hash)) {
        # Differently to `dodgr`, these hashes need to be reproducible, and so
        # are hard-coded to the OSM `object_` values.
        if (!"object_" %in% names (graph)) {
            stop ("graph must be a 'dodgr_streetnet_sc'-class object")
        }
        hash <- digest::digest (list (graph$object_, names (graph)))
    }

    return (hash)
}

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

        if (!fs::dir_exists (cache_dir)) {
            fs::dir_create (cache_dir, recurse = TRUE)
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
#' @param graph A \pkg{silicate}, "SC", format object containing network data
#' used to generate weighted street networks.
#' @return Single character value with unique hash of given network.
#' @family cache
#' @export
m4ra_network_hash <- function (graph) {

    checkmate::assert_class (graph, "osmdata_sc")

    full_hash <- attr (graph, "hash")
    if (!is.null (full_hash)) {
        return (full_hash)
    }

    ids <- list (
        nodes = graph$nodes$vertex_,
        obj = graph$object$object_,
        edge = graph$edge$edge_,
        vertex = graph$vertex$vertex_,
        meta = graph$meta
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
#' @param graph The full network to be cached.
#' @param city Name of city; used to name cached network files.
#' @family cache
#' @export
m4ra_cache_network <- function (graph, city) {

    flist_out <- NULL

    cache_dir <- fs::path (m4ra_cache_dir (), city)
    city <- gsub ("\\s+", "-", tolower (city))

    # Full graph:
    graph <- graph [graph$component == 1, ]
    flist_out <- c (flist_out, cache_one_graph (graph, city))

    # Contract graph return the graph, so need to manually reconstruct the new
    # file names.
    graph_c <- m4ra_contract_graph (graph, city)
    hash <- substring (attr (graph, "hash"), 1, 6)
    flist_out <- fs::dir_ls (cache_dir, regexp = hash, fixed = TRUE)

    return (flist_out)
}

get_graph_attributes <- function (graph) {

    a <- names (attributes (graph))
    index <- which (!a %in% c ("names", "row.names", "col.names", "px"))

    return (attributes (graph) [index])
}

cache_one_graph <- function (graph, city) {

    contracted <- inherits (graph, "dodgr_contracted")

    atag <- ifelse (contracted, "attrc", "attr")
    gtag <- ifelse (contracted, "graphc", "graph")

    a <- get_graph_attributes (graph)
    hash <- substring (a$hash, 1, 6)
    mode <- a$wt_profile

    cache_dir <- fs::path (m4ra_cache_dir (), city)

    aname <- paste0 (
        "m4ra-", city, "-", mode, "-",
        atag, "-", hash, ".Rds"
    )
    apath <- fs::path (cache_dir, aname)

    saveRDS (a, apath)

    gpath <- gsub (
        paste0 ("\\-", atag, "\\-"),
        paste0 ("-", gtag, "-"),
        apath
    )
    fst::write_fst (graph, gpath)

    return (c (apath, gpath))
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

    ptn <- paste0 ("\\-", ifelse (contracted, "graphc", "graph"), "\\-")

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

    graph <- m_read_fst (f)

    ptn <- gsub ("graph", "attr", ptn) # retains 'attrc' for contracted
    a <- readRDS (grep (ptn, flist, value = TRUE))
    for (i in seq_along (a)) {
        attr (graph, names (a) [i]) <- a [[i]]
    }

    return (graph)
}

m_readRDS <- memoise::memoise (function (path) readRDS (path))

m_read_fst <- memoise::memoise (function (path) fst::read_fst (path))
