test_that ("reverse_graph swaps edge directions", {

    g <- data.frame (
        .vx0 = c ("a", "b"), .vx1 = c ("b", "c"),
        .vx0_x = c (0, 1), .vx0_y = c (0, 0),
        .vx1_x = c (1, 2), .vx1_y = c (0, 0),
        d = c (1, 2),
        check.names = FALSE
    )
    r <- reverse_graph (g)
    expect_equal (r$.vx0, g$.vx1)
    expect_equal (r$.vx1, g$.vx0)
    expect_equal (r$.vx0_x, g$.vx1_x)
    expect_equal (r$.vx1_y, g$.vx0_y)
    expect_equal (r$d, g$d)
    expect_equal (reverse_graph (r), g)
})

test_that ("graph_with_times uses times for distances", {

    g <- dodgr::dodgr_contract_graph (dodgr::weight_streetnet (
        dodgr::hampi,
        wt_profile = "foot"
    ))
    gt <- graph_with_times (g)
    expect_equal (gt$d, g$time)
    expect_equal (gt$d_weighted, g$time_weighted)
    expect_equal (attr (gt, "turn_penalty"), 0)
})

test_that ("multi-modal times with two motorcar legs are rejected", {

    expect_error (
        m4ra_times_multi_mode (
            city_name = "test", initial_mode = "motorcar",
            final_mode = "motorcar", from = "a"
        ),
        "can not both be 'motorcar'"
    )
    expect_error (
        m4ra_times_multi_mode (
            city_name = "test", initial_mode = "plane", from = "a"
        )
    )
})
