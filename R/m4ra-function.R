#' Many-to-many multi-modal routing function
#'
#' Perform a many-to-many multi-modal routing query
#'
#' @param net An urban network in `osmdata_sc` format from the \pkg{osmdata}
#' package.
#' @param gtfs A GTFS feed in `gtfs` format from the \pkg{gtfsrouter} package.
#' @param from A vector of start points for routing, specified as Open Street
#' Map (OSM) identifier ("ID") values, generally from the ID column of
#' `dodgr_vertices(net)`, or obtained by matching coordinates to the network
#' with the \pkg{dodgr} `match_pts_to_verts` function.
#' @param to An optional vector of end points for routing.
#' @return (Unsure at this stage)
#' @export
m4ra <- function (net, gtfs, from = NULL, to = NULL) {

    checkmate::assert_class (net, "osmdata_sc")
    checkmate::assert_class (gtfs, "gtfs")
    checkmate::assert_character (from)
    if (!is.null (to)) {
        checkmate::assert_character (to)
    }

    m4ra_dir <- m4ra_cache_dir ()
}
