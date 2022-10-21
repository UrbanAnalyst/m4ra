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

#' Load cached file for one city and mode
#'
#' @param city City for which file is to be loaded.
#' @param mode One of "foot", "bicycle", or "motorcar"
#' @return Previously cached, weighted streetnet for specified city and mode.
#' @family cache
#' @export
m4ra_load_cached_network <- function (city = NULL, mode = "foot") {

    checkmate::assert_character (city, max.len = 1L)
    checkmate::assert_character (mode, max.len = 1L)

    mode <- match.arg (tolower (mode), c ("foot", "bicycle", "motorcar"))

    flist <- list.files (m4ra_cache_dir (), full.names = TRUE)
    flist <- grep (city, flist, value = TRUE)
    f <- grep (mode, flist, value = TRUE)
    if (length (f) != 1L) {
        stop ("No single file found for [city, mode] = [", city, ", ", mode, "]")
    }

    graph <- fst::read_fst (f)

    class (graph) <- c ("dodgr_streetnet_sc", class (graph))
    attr (graph, "wt_profile") <- mode
    attr (graph, "left_side") <- FALSE
    attr (graph, "turn_penalty") <- ifelse (mode == "motorcar", 1., 0.)

    return (graph)
}
