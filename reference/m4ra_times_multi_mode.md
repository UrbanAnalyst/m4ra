# Calculate matrix of pair-wise travel times between points using multiple modes of transport.

Calculate matrix of pair-wise travel times between points using multiple
modes of transport.

## Usage

``` r
m4ra_times_multi_mode(
  net_sc = NULL,
  gtfs = NULL,
  city_name = NULL,
  day = NULL,
  start_time_limits = NULL,
  initial_mode = "foot",
  final_mode = "foot",
  from = NULL,
  duration_max = NULL,
  fast = FALSE,
  n_closest = 10L,
  quiet = FALSE
)
```

## Arguments

- net_sc:

  Local file path to a silicate, "SC", format object containing network
  data used to generate weighted street networks.

- gtfs:

  Local file path to a gtfsrouter object saved via `saveRDS`, containing
  GTFS (General Transit Feed Specification) data for network represented
  in `net_sc`. This `.Rds` object may include additional timetable or
  transfer information in addition to data represented in original
  `zip`-format data of provided GTFS feed.

- city_name:

  Name of city used to name cached files.

- day:

  As for the 'gtfs_traveltimes' function of gtfsrouter, the day for
  which the matrix is to be calculated.

- start_time_limits:

  As for the 'gtfs_traveltimes' function of gtfsrouter, a vector of two
  values specifying the earliest and latest departure times from each
  station.

- initial_mode:

  Initial mode of transport from origin points towards public transport
  stop.

- final_mode:

  The mode of transport used for the final stage from GTFS stops to
  destination points.

- from:

  List of OSM vertex IDs from which to calculate total multi-modal
  travel times. These must be vertices from the largest connected
  component of the contracted graph.

- duration_max:

  If specified, only calculate times from each point to the nearest GTFS
  stops out to specified maximal duration in seconds. Values may be
  provided if overall travel times are only of interest out to some
  defined range or maximal value. Specifying values for this parameter
  can can considerably reduce calculation times.

- fast:

  Values of `TRUE` generate potentially enormous matrices which may not
  fit in local memory. The default value of `FALSE` is generally safer,
  but calculation may take considerably longer.

- n_closest:

  Final travel times to each destination point are calculated by tracing
  back times to this number of closest GTFS stops. Lower values will
  result in faster calculation times, yet with potentially inaccurate
  results.

- quiet:

  If `FALSE`, display progress information on screen.

## See also

Other main:
[`m4ra_dists_n_pts()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_dists_n_pts.md),
[`m4ra_gtfs_traveltimes()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_gtfs_traveltimes.md),
[`m4ra_times_single_mode()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_single_mode.md)
