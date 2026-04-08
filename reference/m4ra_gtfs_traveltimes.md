# Construct a travel time matrix between all pairs of stops in a 'GTFS' feed.

Construct a travel time matrix between all pairs of stops in a 'GTFS'
feed.

## Usage

``` r
m4ra_gtfs_traveltimes(
  gtfs,
  start_time_limits,
  day,
  from_stops = NULL,
  next_interval = TRUE,
  quiet = FALSE
)
```

## Arguments

- gtfs:

  A 'GTFS' feed extracted with the gtfsrouter function, 'extract_gtfs'.

- start_time_limits:

  As for the 'gtfs_traveltimes' function of gtfsrouter, a vector of two
  values specifying the earliest and latest departure times from each
  station.

- day:

  As for the 'gtfs_traveltimes' function of gtfsrouter, the day for
  which the matrix is to be calculated.

- from_stops:

  If not `NULL` (default), calculate travel times only from the stops
  identified in this parameter. All values must be stops taken from
  `gtfs$stops$stop_id`.

- next_interval:

  If `TRUE`, calculate time intervals to subsequent trips after
  identified fastest trips. These subsequent trips may not necessarily
  be as fast as the initial trips, and so this requires a second
  calculation of all travel times. Setting this parameter to `TRUE` will
  therefore generally double the calculation time. Note that intervals
  to subsequent trips may be negative where alternative connections with
  greater numbers of transfers leave earlier than initial,
  minimal-transfer trips.

- quiet:

  If `FALSE`, display progress information on screen.

## Value

A list of two or three integer matrices:

- "duration": The fastest travel times between all pairs of stops for
  the specified 'start_time_limits'; and

- "ntransfers": The corresponding numbers of transfers.

- (Only if 'next_interval = TRUE') "intervals": a matrix of intervals
  (in seconds) until the next fastest service after that corresponding
  to the times in the 'duration' item.

## See also

Other main:
[`m4ra_dists_n_pts()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_dists_n_pts.md),
[`m4ra_times_multi_mode()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_multi_mode.md),
[`m4ra_times_single_mode()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_single_mode.md)
