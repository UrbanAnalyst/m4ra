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
#' @export
m4ra_network_hash <- function (net) {

    checkmate::assert_class (net, "osmdata_sc")

    full_hash <- digest::digest (net)
    hash <- substr (full_hash, 1L, 6L)

    return (hash)
}
