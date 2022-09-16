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

    index_no_start <- one_row_col_index (d, "start")
    index_no_end <- one_row_col_index (d, "end")
    d <- d [-index_no_start, -index_no_end]

    rownames (d) <- gsub ("\\_(start|end)$", "", rownames (d))
    colnames (d) <- gsub ("\\_(start|end)$", "", colnames (d))

    return (d)
}

#' Remove compound junctions from rows/cols of time matrices
#'
#' Matrices have row and column entries representing "_start" and "_end" points
#' of compound junctions, as well as potentially un-compounded versions of same
#' vertices. Travel times to or from these won't have been adjusted for turn
#' penalties or waiting times, so will be unrealistically short and need to be
#' removed.
#'
#' @param d Travel time matrix.
#' @return Reduced version of 'd' with compound junction vertices removed.
#' @noRd
one_row_col_index <- function (d, what = "start") {

    what <- match.arg (what, c ("start", "end"))
    if (what == "start") {
        whatnot <- "\\_end$"
        nms <- rownames (d)
    } else {
        nms <- colnames (d)
        whatnot <- "\\_start$"
    }

    # index of "end" from start pts, or vice-versa:
    index <- grep (whatnot, nms) 

    # then index of pts which should be start or end pts, but occur without
    # suffix. These will generally have times, but they won't be adjusted for
    # turn penalties or wait times, so will be unrealistically short and need
    # to be removed.
    ptn <- paste0 ("\\_", what, "$")
    index1 <- grep (ptn, nms)
    no_what <- gsub (ptn, "", nms [index1])
    # those are plain names which then have to located within nms:
    no_what <- grep (paste0 (no_what, collapse = "|"), nms, value = TRUE)
    no_what <- no_what [which (!grepl ("\\_(start|end)$", no_what))]
    index_no_what <- match (no_what, nms)

    sort (unique (c (index, index_no_what)))
}
