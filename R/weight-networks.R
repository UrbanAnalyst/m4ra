#' Cache weighted networks for routing query
#'
#' Uses a default cache location specified by `rappdirs::user_cache_dir()`. This
#' location can be over-ridden by specifying a local environment variable,
#' "M4RA_CACHE_DIR".
#'
#' @inheritParams m4ra
#' @return A character vector of local locations of cached versions of the
#' variously weighted network representations used in \link{m4ra}.
#' @family cache
#' @export
m4ra_weight_networks <- function (net, quiet = TRUE) {

    requireNamespace ("dplyr")
    requireNamespace ("geodist")

    wt_profiles <- c ("foot", "bicycle")

    filenames <- cache_networks (net,
        wt_profiles = wt_profiles, quiet = quiet)

    return (filenames)
}

cache_networks <- function (net, wt_profiles, quiet = TRUE) {

    hash <- m4ra_network_hash (net)

    filenames <- NULL

    for (w in wt_profiles) {

        filename <- file.path (
            m4ra_cache_dir (),
            paste0 ("m4ra-", hash, "-", w, ".Rds")
        )

        if (!file.exists (filename)) {

            if (!quiet) {
                cli::cli_alert_info ("Weighting network with '{w}' profile")
            }
            net_w <- dodgr::weight_streetnet (net, wt_profile = w)

            fst::write_fst (net_w, filename)
        }

        filenames <- c (filenames, filename)
    }

    return (filenames)
}
