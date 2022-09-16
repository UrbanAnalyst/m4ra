# Internal dodgr functions called directly here:

tbl_to_df <- utils::getFromNamespace ("tbl_to_df", "dodgr")

graph_cols <- utils::getFromNamespace ("dodgr_graph_cols", "dodgr")

get_turn_penalty <- utils::getFromNamespace ("get_turn_penalty", "dodgr")

create_compound_junctions <-
    utils::getFromNamespace ("create_compound_junctions", "dodgr")

make_vert_map <- utils::getFromNamespace ("make_vert_map", "dodgr")

get_to_from_index <- utils::getFromNamespace ("get_to_from_index", "dodgr")

to_from_with_tp <- utils::getFromNamespace ("to_from_with_tp", "dodgr")
