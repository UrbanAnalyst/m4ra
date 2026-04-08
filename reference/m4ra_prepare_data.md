# Prepare network and GTFS components for 'm4ra' queries.

This is a meta-function that calls the following functions:

- [m4ra_weight_networks](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_weight_networks.md)
  to generate several versions of the input street network weighted for
  different kinds of transport.

- [m4ra_gtfs_traveltimes](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_gtfs_traveltimes.md)
  to generate

It also identifies the closest GTFS stops to every network point. The
function stores several differently-weighted version of the street
networks in a local cache directory defined by the `user_cache_dir()`
function of the rappdirs package, or by a local environment parameter,
"M4RA_CACHE_DIR", if set.

## Usage

``` r
m4ra_prepare_data(
  net_sc = NULL,
  gtfs = NULL,
  city_name = NULL,
  planet_file = NULL,
  day = NULL,
  start_time_limits = NULL,
  final_mode = "foot",
  fast = FALSE,
  n_closest = 10L,
  batch_size = NULL,
  parking = FALSE,
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

- planet_file:

  Optional file path to local `.osm.pbf` or `.osm.bz2` file encompassing
  specified bounding box. If given, data are extracted with system-level
  calls to "osmium", which must be installed.

- day:

  As for the 'gtfs_traveltimes' function of gtfsrouter, the day for
  which the matrix is to be calculated.

- start_time_limits:

  As for the 'gtfs_traveltimes' function of gtfsrouter, a vector of two
  values specifying the earliest and latest departure times from each
  station.

- final_mode:

  The mode of transport used for the final stage from GTFS stops to
  destination points.

- fast:

  Values of `TRUE` generate potentially enormous matrices which may not
  fit in local memory. The default value of `FALSE` is generally safer,
  but calculation may take considerably longer.

- n_closest:

  Final travel times to each destination point are calculated by tracing
  back times to this number of closest GTFS stops. Lower values will
  result in faster calculation times, yet with potentially inaccurate
  results.

- batch_size:

  Default routine calculates full travel time matrices between all pairs
  of stops. For very large GTFS feeds, this matrix may not fit into
  memory, in which case this parameter can be set to a positive integer
  value to sequentially calculate only portions of the full matrix,
  before final re-assembly into single result.

- parking:

  If `TRUE`, calculate local densities of parking availability and
  building volumes, and convert to a score used to calculate time
  penalties for automobile routing.

- quiet:

  If `FALSE`, display progress information on screen.

## See also

Other prep:
[`m4ra_parking()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_parking.md),
[`m4ra_times_to_gtfs_stops()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_to_gtfs_stops.md)
