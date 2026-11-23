test_that ("repeated prepare data does not read inputs or recalculate", {

    Sys.setenv ("M4RA_CACHE_DIR" = fs::path_temp ())
    Sys.setenv ("M4RA_NUM_CORES" = 1L)

    z <- gtfsrouter::berlin_gtfs_to_zip ()
    suppressWarnings (
        gtfs <- gtfsrouter::extract_gtfs (z)
    )
    gtfs_path <- fs::path (fs::path_temp (), "cachetest-gtfs.Rds")
    saveRDS (gtfs, gtfs_path)

    net_sc <- m4ra_hampi
    x0 <- min (gtfs$stops$stop_lon)
    xlim <- range (gtfs$stops$stop_lon)
    y0 <- min (gtfs$stops$stop_lat)
    ylim <- range (gtfs$stops$stop_lat)
    net_sc$vertex$x_ <- (net_sc$vertex$x_ - min (net_sc$vertex$x_)) /
        diff (range (net_sc$vertex$x_)) * diff (xlim) + x0
    net_sc$vertex$y_ <- (net_sc$vertex$y_ - min (net_sc$vertex$y_)) /
        diff (range (net_sc$vertex$y_)) * diff (ylim) + y0
    net_sc_path <- fs::path (fs::path_temp (), "cachetest-sc.Rds")
    saveRDS (net_sc, net_sc_path)

    prepare <- function (final_mode = "foot") {
        suppressMessages (m4ra_prepare_data (
            net_sc = net_sc_path,
            gtfs = gtfs_path,
            city_name = "cachetest",
            day = "monday",
            start_time_limits = 12:13 * 3600,
            final_mode = final_mode,
            n_closest = 10L,
            quiet = TRUE
        ))
    }

    f1 <- prepare ()
    expect_length (list.files (
        m4ra_cache_dir ("cachetest"),
        pattern = "\\-input\\-(net|gtfs)\\-.*\\.txt$"
    ), 2L)

    # The same call again reads no files, including with 'readRDS' directly,
    # and calculates nothing:
    nothing <- function (...) stop ("should not be called", call. = FALSE)
    local_mocked_bindings (
        m_readRDS = nothing,
        m4ra_weight_networks = nothing,
        times_gtfs_to_gtfs = nothing
    )
    # (Traced only during the call, because loading packages reads with it)
    suppressMessages (trace (
        "readRDS", quote (stop ("readRDS called", call. = FALSE)),
        print = FALSE, where = baseenv ()
    ))
    f2 <- tryCatch (prepare (), finally = suppressMessages (
        untrace ("readRDS", where = baseenv ())
    ))
    # (The first call lists the networks as it writes them, and later calls as
    # found in the cache directory, which can include more files.)
    expect_true (all (f1 %in% f2))
    expect_true (all (fs::file_exists (f2)))
    expect_identical (
        grep ("gtfs", f2, value = TRUE), grep ("gtfs", f1, value = TRUE)
    )
})

test_that ("prepare data recalculates when inputs change or cached files go", {

    Sys.setenv ("M4RA_CACHE_DIR" = fs::path_temp ())
    Sys.setenv ("M4RA_NUM_CORES" = 1L)

    gtfs_path <- fs::path (fs::path_temp (), "cachetest-gtfs.Rds")
    net_sc_path <- fs::path (fs::path_temp (), "cachetest-sc.Rds")
    skip_if_not (fs::file_exists (gtfs_path) && fs::file_exists (net_sc_path))

    n_weight <- 0L
    n_gtfs <- 0L
    weight_networks <- m4ra_weight_networks
    gtfs_to_gtfs <- m4ra:::times_gtfs_to_gtfs
    local_mocked_bindings (
        m4ra_weight_networks = function (...) {
            n_weight <<- n_weight + 1L
            weight_networks (...)
        },
        times_gtfs_to_gtfs = function (...) {
            n_gtfs <<- n_gtfs + 1L
            gtfs_to_gtfs (...)
        }
    )
    prepare <- function () {
        suppressMessages (m4ra_prepare_data (
            net_sc = net_sc_path,
            gtfs = gtfs_path,
            city_name = "cachetest",
            day = "monday",
            start_time_limits = 12:13 * 3600,
            final_mode = "foot",
            n_closest = 10L,
            quiet = TRUE
        ))
    }

    prepare ()
    expect_equal (c (n_weight, n_gtfs), c (0L, 0L))

    # A changed network file is read and weighted again, but the GTFS is not:
    Sys.setFileTime (net_sc_path, Sys.time () + 100)
    prepare ()
    expect_equal (c (n_weight, n_gtfs), c (1L, 0L))
    prepare ()
    expect_equal (c (n_weight, n_gtfs), c (1L, 0L))

    # Likewise a changed GTFS file:
    Sys.setFileTime (gtfs_path, Sys.time () + 100)
    prepare ()
    expect_equal (c (n_weight, n_gtfs), c (1L, 1L))

    # Removed cached files are calculated again:
    cache_dir <- m4ra_cache_dir ("cachetest")
    fs::file_delete (fs::dir_ls (
        cache_dir,
        regexp = "gtfs\\-[0-9a-f]+\\-(times|transfers|intervals)\\-mo\\-"
    ))
    prepare ()
    expect_equal (c (n_weight, n_gtfs), c (1L, 2L))

    # as are the networks, if only these are removed (including the flag):
    fs::file_delete (fs::dir_ls (cache_dir, regexp = "\\-done$"))
    prepare ()
    expect_equal (c (n_weight, n_gtfs), c (2L, 2L))
})

test_that ("a partly removed set of GTFS matrices is calculated again", {

    Sys.setenv ("M4RA_CACHE_DIR" = fs::path_temp ())
    Sys.setenv ("M4RA_NUM_CORES" = 1L)

    gtfs_path <- fs::path (fs::path_temp (), "cachetest-gtfs.Rds")
    net_sc_path <- fs::path (fs::path_temp (), "cachetest-sc.Rds")
    skip_if_not (fs::file_exists (gtfs_path) && fs::file_exists (net_sc_path))

    cache_dir <- m4ra_cache_dir ("cachetest")
    prepare <- function () {
        suppressMessages (m4ra_prepare_data (
            net_sc = net_sc_path,
            gtfs = gtfs_path,
            city_name = "cachetest",
            day = "monday",
            start_time_limits = 12:13 * 3600,
            final_mode = "foot",
            n_closest = 10L,
            quiet = TRUE
        ))
    }
    ptn <- "gtfs\\-[0-9a-f]+\\-(times|transfers|intervals)\\-mo\\-"
    all_files <- function () fs::dir_ls (cache_dir, regexp = ptn)

    prepare ()
    expect_length (all_files (), 3L)
    for (what in c ("times", "transfers", "intervals")) {
        f <- grep (paste0 ("\\-", what, "\\-"), all_files (), value = TRUE)
        fs::file_delete (f)
        expect_length (all_files (), 2L)
        expect_error (prepare (), NA)
        expect_length (all_files (), 3L)
    }

    # and if the markers are also removed, so that nothing is known of them:
    f <- grep ("\\-times\\-", all_files (), value = TRUE)
    fs::file_delete (f)
    fs::file_delete (fs::dir_ls (cache_dir, regexp = "\\-input\\-"))
    expect_error (prepare (), NA)
    expect_length (all_files (), 3L)
})

test_that ("nearest stops are calculated again when the stops or network change", {

    Sys.setenv ("M4RA_CACHE_DIR" = fs::path_temp ())
    Sys.setenv ("M4RA_NUM_CORES" = 1L)

    gtfs_path <- fs::path (fs::path_temp (), "cachetest-gtfs.Rds")
    net_sc_path <- fs::path (fs::path_temp (), "cachetest-sc.Rds")
    skip_if_not (fs::file_exists (gtfs_path) && fs::file_exists (net_sc_path))

    cache_dir <- m4ra_cache_dir ("cachetest")
    prepare <- function (mode = "foot", gtfs = gtfs_path) {
        suppressMessages (m4ra_prepare_data (
            net_sc = net_sc_path,
            gtfs = gtfs,
            city_name = "cachetest",
            day = "monday",
            start_time_limits = 12:13 * 3600,
            final_mode = mode,
            n_closest = 10L,
            quiet = TRUE
        ))
    }
    to_net <- function (mode = "foot") {
        fs::dir_ls (cache_dir, regexp = paste0 ("gtfs\\-to\\-net\\-", mode, "\\-"))
    }

    prepare ()
    f1 <- to_net ()
    expect_length (f1, 1L)

    # Nothing changed: the same file, which is not even listed again
    expect_identical (to_net (), f1)
    # Another mode has its own file, and leaves this one
    prepare ("bicycle")
    expect_length (to_net ("bicycle"), 1L)
    expect_identical (to_net (), f1)

    # Changed stops give a file for those stops, and remove the old one. (The
    # changed feed is in a new file, because files are memoised by path.)
    gtfs <- readRDS (gtfs_path)
    gtfs$stops$stop_name [1] <- paste (gtfs$stops$stop_name [1], "(changed)")
    gtfs_path2 <- fs::path (fs::path_temp (), "cachetest-gtfs-changed.Rds")
    saveRDS (gtfs, gtfs_path2)
    prepare (gtfs = gtfs_path2)
    f2 <- to_net ()
    expect_length (f2, 1L)
    expect_false (identical (f1, f2))
    expect_length (to_net ("bicycle"), 1L)
})
