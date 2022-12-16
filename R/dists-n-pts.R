#' Calculate network distances to nearest 'n' points.
#'
#' @param graph A `dodgr` network returned from the \link{weight_streetnet}
#' function using a network obtained with the \pkg{osmdata} `osmdata_sc`
#' function.
#' @param from Vector of OpenStreetMap ID values from which route distances are
#' to be calculated.
#' @param to Vector of OpenStreetMap ID values to which nearest 'npts' distances are
#' to be calculated.
#' @param npts Number of points to which distances are to be calculated.
#'
#' @family main
#' @export
m4ra_dists_n_pts <- function (graph,
                              from = NULL,
                              to = NULL,
                              npts = 3L) {

    if (!methods::is (graph, "dodgr_streetnet_sc")) {
        stop ("'graph' must be a 'dodgr_streetnet_sc' object")
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

    # Set turn_penalty = 0, because turn penalties have been calcualted in the
    # cached versions of graphs. This enables `from_from_index_with_tp` to
    # return full indices into the contracted + compound junction versions of
    # the graphs stored in cache.
    attr (graph, "turn_penalty") <- 0
    to_from_indices <- to_from_index_with_tp (graph, from, to)

    dmat <- rcpp_dists_to_n_targets (
        graph,
        to_from_indices$vert_map,
        to_from_indices$from$index,
        to_from_indices$to$index,
        npts
    )

    # The 1st half of dmat (dmat [, seq_len (npts)]) holds distances, and the
    # second half (dmat [, npts + seq_len (npts)]) holds indices into the
    # vertices of the network.
    index <- seq_len (npts) + npts
    imat <- dmat [, index]
    storage.mode (imat) <- "integer"

    dmat <- dmat [, seq_len (npts)]

    return (list (dist_mat = dmat, index_mat = imat))
}
