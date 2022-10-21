#' Calculate matrix of pair-wise travel times between points using a single mode
#' of transport.
#'
#' @param graph A `dodgr` network returned from the \link{weight_streetnet}
#' function using a network obtained with the \pkg{osmdata} `osmdata_sc`
#' function.
#' @param from Vector or matrix of points **from** which route distances are to
#' be calculated (see Notes)
#' @param to Vector or matrix of points **to** which route distances are to be
#' calculated (see Notes)
#' @param path If specified, save individual travel time vectors for each 'from'
#' point at that local directory.
#'
#' `from` and `to` values can be either two-column matrices or
#' equivalent of longitude and latitude coordinates, or else single columns
#' precisely matching node numbers or names given in `graph$from` or
#' `graph$to`. If `to` is `NULL`, pairwise distances are calculated
#' between all points specified in `from`. If both `from` and `to` are `NULL`,
#' pairwise distances are calculated between all nodes in `graph`.
#'
#' @family main
#' @examples
#' net <- dodgr::weight_streetnet (m4ra_hampi, wt_profile = "foot")
#' traveltimes <- m4ra_times_single_mode (net)
#' @export
m4ra_times_single_mode <- function (graph,
                                    from = NULL,
                                    to = NULL,
                                    path = NULL) {

    if (!methods::is (graph, "dodgr_streetnet_sc")) {
        stop ("'graph' must be a 'dodgr_streetnet_sc' object")
    }
    if (!is.null (path)) {
        checkmate::assert_character (path)
        checkmate::assert_directory_exists (path)
    }

    graph <- tbl_to_df (graph)

    gr_cols <- dodgr_graph_cols (graph)
    if (is.na (gr_cols$time)) {
        stop ("graph has no time column")
    }
    if (is.na (gr_cols$time_weighted)) {
        stop (
            "Graph does not contain a weighted time column from ",
            "which to calculate fastest paths."
        )
    }

    vert_map <- make_vert_map (graph, gr_cols, xy = TRUE)
    from_index <-
        get_to_from_index (graph, vert_map, gr_cols, from, from = TRUE)
    to_index <-
        get_to_from_index (graph, vert_map, gr_cols, to, from = FALSE)

    if (get_turn_penalty (graph) > 0.0) {
        if (methods::is (graph, "dodgr_contracted")) {
            warning (
                "graphs with turn penalties should be submitted in full, ",
                "not contracted form;\nsubmitting contracted graphs may ",
                "produce unexpected behaviour."
            )
        }
        res <- create_compound_junctions (graph)
        compound_junctions <- res$edge_map
        graph <- res$graph

        # remap any 'from' and 'to' vertices to compound junction versions:
        is_spatial <- TRUE
        vert_map <- make_vert_map (graph, gr_cols, is_spatial)

        from_index <- remap_tf_index_for_tp (from_index, vert_map, from = TRUE)
        to_index <- remap_tf_index_for_tp (to_index, vert_map, from = FALSE)
    }
    graph [[gr_cols$d]] <- graph [[gr_cols$time]]
    graph [[gr_cols$d_weighted]] <- graph [[gr_cols$time_weighted]]


    if (is.null (path)) {

        d <- calculate_timemat (
            graph,
            vert_map,
            from_index,
            to_index
        )

    } else {

        d <- save_time_vecs (
            graph,
            vert_map,
            from_index,
            to_index,
            path
        )
    }

    return (d)
}

calculate_timemat <- function (graph,
                               vert_map,
                               from_index,
                               to_index) {

    d <- rcpp_get_sp_dists_par (
        graph,
        vert_map,
        from_index$index,
        to_index$index
    )

    if (!is.null (from_index$id)) {
        rownames (d) <- from_index$id
    } else {
        rownames (d) <- vert_map$vert
    }
    if (!is.null (to_index$id)) {
        colnames (d) <- to_index$id
    } else {
        colnames (d) <- vert_map$vert
    }

    if (get_turn_penalty (graph) > 0) {

        rownames (d) <- gsub ("\\_(start|end)$", "", rownames (d))
        colnames (d) <- gsub ("\\_(start|end)$", "", colnames (d))
    }

    return (d)
}

save_time_vecs <- function (graph,
                            vert_map,
                            from_index,
                            to_index,
                            path) {

    path <- normalizePath (path)
    path_end <- substr (path, nchar (path), nchar (path))
    if (path_end != .Platform$file.sep) {
        path <- paste0 (path, .Platform$file.sep)
    }

    flist <- list.files (path)
    if (length (flist) > 0L) {
        stop ("path '", path, "' must be an empty directory.")
    }

    from_names <- gsub ("\\_start$", "", from_index$id)

    chk <- rcpp_save_sp_dists_par (
        graph,
        vert_map,
        from_index$index,
        from_names,
        to_index$index,
        path
    )

    flist <- list.files (path, full.names = TRUE)
    flist_from_names <- gsub ("^m4ra\\_from\\_", "", basename (flist))
    index <- match (from_names, flist_from_names)

    return (flist [index])
}
