
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

test_that ("gtfs travel times", {

    z <- gtfsrouter::berlin_gtfs_to_zip ()
    gtfs <- gtfsrouter::extract_gtfs (z)
    gtfs <- gtfsrouter::gtfs_timetable (gtfs, day = "Monday")

    start_time_limits <- 12:13 * 3500
    tt <- m4ra_gtfs_traveltimes (gtfs, start_time_limits = start_time_limits)

    expect_type (tt, "double")
    expect_equal (nrow (tt), ncol (tt))
    expect_true (length (which (is.na (tt))) > 0)
    expect_true (length (which (!is.na (tt))) > 0)
})
