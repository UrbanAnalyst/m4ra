
get_hash <- function (graph, contracted = FALSE, force = FALSE) {

    hash <- NULL
    if (!force) {
        hash <- attr (graph, ifelse (contracted, "hashc", "hash"))
    }

    if (is.null (hash)) {
        gr_cols <- dodgr_graph_cols (graph)
        hash <- digest::digest (list (graph [[gr_cols$edge_id]], names (graph)))
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
#' @param net A \pkg{silicate}, "SC", format object containing network data used
#' to generate weighted street networks.
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
    flist_out <- c (flist_out, cache_one_graph (net, city))

    # Contracted graph:
    netc <- dodgr::dodgr_contract_graph (net)
    netc <- netc [which (netc$component == 1), ]
    a_netc <- attributes (netc)

    if (a$turn_penalty > 0) {

        to_from_indices <- to_from_index_with_tp (net, from = NULL, to = NULL)
        if (to_from_indices$compound) {
            netc <- to_from_indices$graph_compound
            a_netc <- a_netc [which (!names (a_netc) %in% names (attributes (netc)))]
            for (a in seq_along (a_netc)) {
                attr (netc, names (a_netc) [a]) <- a_netc [[a]]
            }
        }
    }

    flist_out <- c (flist_out, cache_one_graph (netc, city))

    # Edge map and junctions:
    flist <- fs::dir_ls (fs::path_temp (), regexp = hashc, fixed = TRUE)
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

get_graph_attributes <- function (graph) {

    a <- names (attributes (graph))
    index <- which (!a %in% c ("names", "row.names", "col.names", "px"))

    return (attributes (graph) [index])
}

cache_one_graph <- function (graph, city) {

    contracted <- inherits (graph, "dodgr_contracted")

    which_hash <- ifelse (contracted, "hashc", "hash")
    atag <- ifelse (contracted, "attrc", "attr")
    gtag <- ifelse (contracted, "graphc", "graph")

    a <- get_graph_attributes (graph)
    hash <- substring (a [[which_hash]], 1, 6)
    mode <- a$wt_profile

    cache_dir <- fs::path (m4ra_cache_dir (), city)

    aname <- paste0 ("m4ra-", city, "-", mode, "-",
        atag, "-", hash, ".Rds")
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

    graph <- m_read_fst (f)

    ptn <- gsub ("net", "attr", ptn) # retains 'attrc' for contracted
    a <- readRDS (grep (ptn, flist, value = TRUE))
    for (i in seq_along (a)) {
        attr (graph, names (a) [i]) <- a [[i]]
    }

    return (graph)
}

m_readRDS <- memoise::memoise (function (path) readRDS (path))

m_read_fst <- memoise::memoise (function (path) fst::read_fst (path))
