# Internal dodgr functions called directly here:

tbl_to_df <- utils::getFromNamespace ("tbl_to_df", "dodgr")

dodgr_graph_cols <- utils::getFromNamespace ("dodgr_graph_cols", "dodgr")

create_compound_junctions <-
    utils::getFromNamespace ("create_compound_junctions", "dodgr")

make_vert_map <- utils::getFromNamespace ("make_vert_map", "dodgr")

preprocess_spatial_cols <- utils::getFromNamespace ("preprocess_spatial_cols", "dodgr")

is_graph_spatial <- utils::getFromNamespace ("is_graph_spatial", "dodgr")

to_from_index_with_tp <- utils::getFromNamespace ("to_from_index_with_tp", "dodgr")

get_turn_penalty <- utils::getFromNamespace ("get_turn_penalty", "dodgr")

get_hash <- utils::getFromNamespace ("get_hash", "dodgr")
