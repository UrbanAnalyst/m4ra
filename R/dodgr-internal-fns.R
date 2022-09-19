# Internal dodgr functions called directly here:

tbl_to_df <- utils::getFromNamespace ("tbl_to_df", "dodgr")

graph_cols <- utils::getFromNamespace ("dodgr_graph_cols", "dodgr")

#create_compound_junctions <-
#    utils::getFromNamespace ("create_compound_junctions", "dodgr")

make_vert_map <- utils::getFromNamespace ("make_vert_map", "dodgr")

get_to_from_index <- utils::getFromNamespace ("get_to_from_index", "dodgr")

are_turns_restricted <- utils::getFromNamespace ("are_turns_restricted", "dodgr")

# This is only in versions > v0.2.15
# get_turn_penalty <- utils::getFromNamespace ("get_turn_penalty", "dodgr")
get_turn_penalty <- function (graph) {
    tp <- attr (graph, "turn_penalty")
    if (!is.numeric (tp)) {
        tp <- 0.0
    }
    return (tp)
}

#' Create compound junctions representing turn penalties and restrictions
#'
#' @param graph A weighted streetnet with a non-zero "turn_penalty" attribute,
#' and other parameters stored as graph attributes during streetnet
#' construction.
#' @return List of two items: The expanded graph included compound junctions,
#' and an edge map mapping the new compound edges back on to original graph
#' edges.
#' @noRd
create_compound_junctions <- function (graph) {

    left_side <- attr (graph, "left_side")
    wt_profile <- attr (graph, "wt_profile")
    wt_profile_file <- attr (graph, "wt_profile_file")

    res <- join_junctions_to_graph (
        graph, wt_profile, wt_profile_file,
        left_side
    )
    if (are_turns_restricted (wt_profile, wt_profile_file)) {
        res <- remove_turn_restrictions (graph, res)
    }

    return (res)
}

join_junctions_to_graph <- function (graph, wt_profile, wt_profile_file,
                                     left_side = FALSE) {

    get_turn_penalties <- utils::getFromNamespace ("get_turn_penalties", "dodgr")
    rcpp_route_times <- utils::getFromNamespace ("rcpp_route_times", "dodgr")
    turn_penalty <- get_turn_penalties (wt_profile, wt_profile_file)$turn
    resbind <- edge_map <- NULL

    if (turn_penalty > 0) {

        res <- rcpp_route_times (graph, left_side, turn_penalty)
        edge_map <- data.frame (
            "edge" = res$graph$edge_,
            "e_in" = res$graph$old_edge_in,
            "e_out" = res$graph$old_edge_out,
            stringsAsFactors = FALSE
        )
        res$graph$old_edge_in <- res$graph$old_edge_out <- NULL

        index <- which (graph$.vx0 %in% res$junction_vertices)
        graph$.vx0 [index] <- paste0 (graph$.vx0 [index], "_start")
        index <- which (graph$.vx1 %in% res$junction_vertices)
        graph$.vx1 [index] <- paste0 (graph$.vx1 [index], "_end")

        # pad out extra columns of res to match any extra in original graph
        resbind <- data.frame (array (NA, dim = c (
            nrow (res$graph),
            ncol (graph)
        )))
        names (resbind) <- names (graph)
        resbind [, which (names (graph) %in% names (res$graph))] <- res$graph
        graph <- rbind (graph, resbind)
    }
    list (graph = graph, edge_map = edge_map)
}

#' Remove turn restrictions
#'
#' @param x The original `sc` object which ,when generated from
#' `dodgr_streetnet_sc`, includes turn restriction data
#' @param graph The processed by not yet turn-contracted graph
#' @param res The result of `join_junctions_to_graph`, with turn-contracted
#' `graph` and `edge_map` components.
#' @noRd
remove_turn_restrictions <- function (graph, res) {

    # These are the attributes inserted in initial streeetnet construction via
    # the previous `extract_turn_restrictions` function.
    rw_no <- attr (graph, "turn_restriction_no")
    rw_only <- attr (graph, "turn_restriction_only")

    index0 <- match (rw_no$node, graph$.vx1) # in-edges
    index1 <- match (rw_no$node, graph$.vx0) # out-edges
    in_edges <- graph$edge_ [index0 [which (!is.na (index0))]]
    out_edges <- graph$edge_ [index1 [which (!is.na (index1))]]
    index <- which (res$edge_map$e_in %in% in_edges &
        res$edge_map$e_out %in% out_edges)
    no_turn_edges <- res$edge_map$edge [index]

    index0 <- match (rw_only$node, graph$.vx1) # in-edges
    index1 <- match (rw_only$node, graph$.vx0) # out-edges
    in_edges <- graph$edge_ [index0 [which (!is.na (index0))]]
    out_edges <- graph$edge_ [index1 [which (!is.na (index1))]]
    # index of turns to edges other than "only" turn edges, so also to edges
    # which are to be excluded:
    index <- which (res$edge_map$e_in %in% in_edges &
        !res$edge_map$e_out %in% out_edges)
    no_turn_edges <- unique (c (no_turn_edges, res$edge_map$edge [index]))

    res$graph <- res$graph [which (!res$graph$edge_ %in% no_turn_edges), ]
    res$edge_map <- res$edge_map [
        which (!res$edge_map$edge %in% no_turn_edges),
    ]

    return (res)
}
