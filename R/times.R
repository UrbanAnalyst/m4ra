#' m4ra_times
#'
#' Calculate matrix of pair-wise travel times between points.
#'
#' @param graph A `dodgr` network returned from the \link{weight_streetnet}
#' function using a network obtained with the \pkg{osmdata} `osmdata_sc`
#' function.
#' @param from Vector or matrix of points **from** which route distances are to
#' be calculated (see Notes)
#' @param to Vector or matrix of points **to** which route distances are to be
#' calculated (see Notes)
#'
#' `from` and `to` values can be either two-column matrices or
#' equivalent of longitude and latitude coordinates, or else single columns
#' precisely matching node numbers or names given in `graph$from` or
#' `graph$to`. If `to` is `NULL`, pairwise distances are calculated
#' between all points specified in `from`. If both `from` and `to` are `NULL`,
#' pairwise distances are calculated between all nodes in `graph`.
#'
#' @family distances
#' @export
m4ra_times <- function (graph,
                        from = NULL,
                        to = NULL) {

    if (!methods::is (graph, "dodgr_streetnet_sc")) {
        stop ("'graph' must be a 'dodgr_streetnet_sc' object")
    }

    # ------- `dodgr` internal functions:
    tbl_to_df <- utils::getFromNamespace ("tbl_to_df", "dodgr")
    graph_cols <- utils::getFromNamespace ("dodgr_graph_cols", "dodgr")
    get_turn_penalty <- utils::getFromNamespace ("get_turn_penalty", "dodgr")
    create_compound_junctions <-
        utils::getFromNamespace ("create_compound_junctions", "dodgr")
    make_vert_map <- utils::getFromNamespace ("make_vert_map", "dodgr")
    get_to_from_index <- utils::getFromNamespace ("get_to_from_index", "dodgr")
    to_from_with_tp <- utils::getFromNamespace ("to_from_with_tp", "dodgr")
    # ------- end `dodgr` internal functions:

    graph <- tbl_to_df (graph)

    gr_cols <- graph_cols (graph)
    if (is.na (gr_cols$time)) {
        stop ("graph has no time column")
    }

    graph [[gr_cols$d]] <- graph [[gr_cols$time]]

    if (is.na (gr_cols$time_weighted)) {
        stop (
            "Graph does not contain a weighted time column from ",
            "which to calculate fastest paths."
        )
    }
    graph [[gr_cols$d_weighted]] <- graph [[gr_cols$time_weighted]]

    if (get_turn_penalty (graph) > 0.0) {
        if (methods::is (graph, "dodgr_contracted")) {
            warning (
                "graphs with turn penalties should be submitted in full, ",
                "not contracted form;\nsubmitting contracted graphs may ",
                "produce unexpected behaviour."
            )
        }
        graph <- create_compound_junctions (graph)$graph
    }

    vert_map <- make_vert_map (graph, gr_cols, xy = TRUE)

    from_index <- get_to_from_index (graph, vert_map, gr_cols, from)
    to_index <- get_to_from_index (graph, vert_map, gr_cols, to)

    d <- calculate_timemat (
        graph,
        vert_map,
        from_index,
        to_index
    )

    return (d)
}

calculate_timemat <- function (graph,
                               vert_map,
                               from_index,
                               to_index) {

    flip <- FALSE
    if (length (from_index$index) > length (to_index$index)) {
        flip <- TRUE
        graph <- flip_graph (graph)
        temp <- from_index
        from_index <- to_index
        to_index <- temp
    }

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

    if (flip) {
        d <- t (d)
    }

    return (d)
}
