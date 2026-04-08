# Get data on parking availability and building volumes to estimate time penalties for automobile travel.

Get data on parking availability and building volumes to estimate time
penalties for automobile travel.

## Usage

``` r
m4ra_parking(
  bb,
  city_name,
  mode = "motorcar",
  planet_file = NULL,
  dlim = 5000,
  k = 1000,
  quiet = FALSE
)
```

## Arguments

- bb:

  Bounding box of city for query to extract parking data.

- city_name:

  Name of city used to name cached files.

- mode:

  Mode of transport used to extract OSM node IDs at which to estimate
  relative parking availability (should generally be "motorcar").

- planet_file:

  Optional file path to local `.osm.pbf` or `.osm.bz2` file encompassing
  specified bounding box. If given, data are extracted with system-level
  calls to "osmium", which must be installed.

- dlim:

  Distance limit in metres out to which contributions of parking and
  buildings will be aggregated.

- k:

  With of exponential function used to weight contributions with
  distance, `exp(-d / k)` for distance `d`. Default value of 1000 metres
  decreases weight to 37% at 1km, 14% at 2km, and 0.6% at 5km.

- quiet:

  If `FALSE`, display progress information on screen.

## Value

A `data.frame` of the vertices of the (contracted) network, with
additional columns quantifying number of parking spaces associated with
each vertex, as well as the total volume of all surrounding buildings.

## See also

Other prep:
[`m4ra_prepare_data()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_prepare_data.md),
[`m4ra_times_to_gtfs_stops()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_to_gtfs_stops.md)
