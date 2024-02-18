#' m4rar_contract_graph
#'
#' Removes redundant (straight-line) vertices from graph, leaving only junction
#' vertices. This is mostly a copy of the same function in \pkg{dodgr}, but uses
#' \pkg{fst} to write and read locally-cached versions of graphs, does that in
#' the `m4ra_cache_dir` rather than tempdir, and does not expose the `verts`
#' parameter, so graph contraction leaves junction vertices only, with no
#' optional extra vertices.
#'
#' @param graph A flat table of graph edges. Must contain columns labelled
#' `from` and `to`, or `start` and `stop`. May also contain
#' similarly labelled columns of spatial coordinates (for example
#' `from_x`) or `stop_lon`).
#'
#' @return A contracted version of the original `graph`, containing the same
#' number of columns, but with each row representing an edge between two
#' junction vertices (or between the submitted `verts`, which may or may not be
#' junctions).
#' @noRd
m4ra_contract_graph <- function (graph, city) {

    if (nrow (graph) == 0) {
        stop ("graph is empty")
    } # nocov

    # px is the R6 processx object for initial caching
    px <- NULL
    if ("px" %in% names (attributes (graph))) {
        px <- attr (graph, "px")
        while (px$is_alive ()) {
            px$wait ()
        }
    }

    city <- gsub ("\\s+", "-", tolower (city))

    wt_profile <- attr (graph, "wt_profile")

    cache_dir <- fs::path (m4ra_cache_dir (), city)

    v <- m4ra_vertices (graph, city)

    hash <- substring (attr (graph, "hash"), 1, 6)
    fname_c <- fs::path (
        cache_dir,
        paste0 ("m4ra-", city, "-", wt_profile, "-graphc-",
            hash, ".Rds")
    )

    if (fs::file_exists (fname_c)) {

        graph_c <- m4ra_load_cached_network (
            city,
            mode = wt_profile,
            contracted = TRUE
        )

    } else {

        fname <- fs::path (
            cache_dir,
            paste0 ("m4ra-", city, "-", wt_profile, "-graph-",
                hash, ".Rds")
        )
        if (!fs::file_exists (fname)) {
            flist <- cache_one_graph (graph, city)
        }

        graph_contracted <- dodgr_contract_graph_internal (graph, v)

        edge_map <- graph_contracted$edge_map
        graph_c <- graph_contracted$graph
        graph_c <- graph_c [graph_c$component == 1, ]

        a_graph_c <- attributes (graph_c)

        if (a_graph_c$turn_penalty > 0) {

            to_from_indices <- to_from_index_with_tp (graph, from = NULL, to = NULL)
            if (to_from_indices$compound) {

                graph_c <- to_from_indices$graph_compound
                a_graph_c <- a_graph_c [which (!names (a_graph_c) %in%
                    names (attributes (graph_c)))]

                for (a in seq_along (a_graph_c)) {
                    attr (graph_c, names (a_graph_c) [a]) <- a_graph_c [[a]]
                }
            }
        }
        class (graph_c) <- c (class (graph_c), "dodgr_contracted")

        cache_one_graph (graph_c, city)

        fname_e <- fs::path (
            cache_dir,
            paste0 ("m4ra-", city, "-", wt_profile, "-edgemap-",
                substring (hash, 1, 6), ".Rds")
        )
        fst::write_fst (edge_map, fname_e, compress = 0)
    }

    return (graph_c)
}
