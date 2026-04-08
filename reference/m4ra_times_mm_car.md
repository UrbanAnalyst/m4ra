# Calculate relative times from a specified point by multi-modal transport versus private automobile, along with (optionally) corresponding walking distances.

This city expects weighted networks to have been generated with the
[m4ra_batch_weight_networks](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_batch_weight_networks.md)
function, and for corresponding networks to exist in the `m4ra` cache
directory for the specified city.

## Usage

``` r
m4ra_times_mm_car(
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
  walk_dists = TRUE,
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

  Vector of OSM ID values of vertices from which ratios are to be
  calculated. Typically obtained by loading one weighted network, and
  sampling or extracting vertices from the function `m4ra_vertices`.

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

- walk_dists:

  If `TRUE`, also calculate equivalent walking distances.

- quiet:

  If `FALSE`, display progress information on screen.

## Value

A `data.frame` of destination vertices, including Open Street Map ID
values, and longitude and latitude values, and four variables:

- 'car_t' Times in seconds to travel with automobile.

- 'mm_t' Times in seconds for equivalent multi-modal transport.

- 'walk_d' Equivalent walking distance in kilometres.

- 'ratio' Ratio of multi-modal to automobile travel times.

## See also

Other analyses:
[`m4ra_bike_car_ratio_areas()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_bike_car_ratio_areas.md),
[`m4ra_times_bike_car()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_bike_car.md)

## Examples

``` r
if (FALSE) { # \dontrun{
city <- "<city_name>"
net <- m4ra_load_cached_network (city = city, mode = "foot")
v <- m4ra_vertices (net, "<city_name>")
from <- sample (v$id, size = 10)
dat <- m4ra_mm_car_ratios (city = city, from = from)
} # }
```
