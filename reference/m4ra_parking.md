# Get data on parking availability and building volumes to estimate time penalties for automobile travel.

Get data on parking availability and building volumes to estimate time
penalties for automobile travel.

## Usage

``` r
m4ra_parking(
  bb,
  city_name,
  mode = "motorcar",
  osm_extract = NULL,
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

- osm_extract:

  Optional file path to a local OSM data file, such as `.osm.pbf` or
  `.osm.bz2`, encompassing the specified bounding box. If given, data
  are extracted from this file with rosmium, rather than being
  downloaded from the Overpass API. The extract for the bounding box is
  saved in the same directory as `<city_name>.osm.pbf`, and, along with
  files with the filtered data, is used again if it already exists. The
  `osm_extract` file itself is never modified.

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
each vertex (`parking`), as well as the volume of all surrounding
buildings, in cubic metres (`building_volume`). Both are
distance-weighted means over nearby parking sites and buildings,
rescaled so that their means over all vertices are the overall mean
capacity per parking site, and mean volume per building. The `ratio`
column is `parking / building_volume ^ (1 / 3)`, and `penalty_start` and
`penalty_end` are the corresponding time penalties for parking at the
start and end of journeys, in seconds.

## See also

Other prep:
[`m4ra_prepare_data()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_prepare_data.md),
[`m4ra_times_to_gtfs_stops()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_to_gtfs_stops.md)
