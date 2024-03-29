#' m4ra_vertices
#'
#' Extract vertices of graph, including spatial coordinates if included.
#' Modified version of equivalent \pkg{dodgr} function that uses different hash
#' comparisons. This version also does none of the checks implemented in the
#' \pkg{dodgr} version, and assumes graphs remain unmodified throughout.
#'
#' @param graph A flat table of graph edges. Must contain columns labelled
#' `from` and `to`, or `start` and `stop`. May also contain
#' similarly labelled columns of spatial coordinates (for example
#' `from_x` or `stop_lon`).
#' @param city Name of city; used to name cached network files.
#' @return A `data.frame` of vertices with unique numbers (`n`).
#'
#' @note Values of `n` are 0-indexed
#'
#' @family misc
#' @export
#' @examples
#' graph <- dodgr::weight_streetnet (dodgr::hampi)
#' v <- m4ra_vertices (graph, "hampi")
m4ra_vertices <- function (graph, city) {

    cache_dir <- fs::path (m4ra_cache_dir (), city)
    if (!fs::dir_exists (cache_dir)) {
        fs::dir_create (cache_dir, recurse = TRUE)
    }

    hash <- substring (attr (graph, "hash"), 1, 6)
    city <- gsub ("\\s+", "-", tolower (city))
    mode <- attr (graph, "wt_profile")

    contracted <- inherits (graph, "dodgr_contracted")
    vtag <- ifelse (contracted, "vertsc", "verts")

    fname <- paste0 (
        "m4ra-", city, "-", mode,
        "-", vtag, "-", hash, ".Rds"
    )
    fpath <- fs::path (cache_dir, fname)

    if (fs::file_exists (fpath)) {
        verts <- readRDS (fpath)
    } else {
        verts <- dodgr_vertices_internal (graph)
        saveRDS (verts, fpath)
    }

    return (verts)
}
