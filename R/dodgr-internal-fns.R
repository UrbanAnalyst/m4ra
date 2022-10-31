# Internal dodgr functions called directly here:

tbl_to_df <- utils::getFromNamespace ("tbl_to_df", "dodgr")

dodgr_graph_cols <- utils::getFromNamespace ("dodgr_graph_cols", "dodgr")

create_compound_junctions <-
    utils::getFromNamespace ("create_compound_junctions", "dodgr")

make_vert_map <- utils::getFromNamespace ("make_vert_map", "dodgr")

get_to_from_index <- utils::getFromNamespace ("get_to_from_index", "dodgr")

remap_tf_index_for_tp <- utils::getFromNamespace ("remap_tf_index_for_tp", "dodgr")

get_turn_penalty <- utils::getFromNamespace ("get_turn_penalty", "dodgr")

get_hash <- utils::getFromNamespace ("get_hash", "dodgr")
