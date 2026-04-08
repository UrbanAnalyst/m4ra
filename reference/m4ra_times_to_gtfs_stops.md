# Construct a travel time matrix to or from all stops in a 'GTFS' feed from or to to all points in a street network.

Construct a travel time matrix to or from all stops in a 'GTFS' feed
from or to to all points in a street network.

## Usage

``` r
m4ra_times_to_gtfs_stops(graph, gtfs, city, from = NULL, graph_to_gtfs = TRUE)
```

## Arguments

- graph:

  A `dodgr` network returned from the weight_streetnet function using a
  network obtained with the osmdata `osmdata_sc` function.

- gtfs:

  A 'GTFS' feed extracted with the gtfsrouter function, 'extract_gtfs'.

- city:

  Name of city being analysed; used to name and extract cache files.

- from:

  Vector or matrix of points **from** which route distances are to be
  calculated. If not given, times are calculated from all points in the
  network. Only has any effect is `graph_to_gtfs` is `TRUE`.

- graph_to_gtfs:

  If `TRUE`, generate matrix of times from all network junctions in
  'graph' (or all `from` points if specified) to each stop in the
  'gtfs\$stops' table; otherwise generate matrix of times from all stops
  to all network junctions.

## Value

An integer matrix of fastest travel times either between all 'gtfs'
stops and all network points (for 'graph_to_gtfs = FALSE'), or the other
way around (for 'graph_to_gtfs = TRUE').

## See also

Other prep:
[`m4ra_parking()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_parking.md),
[`m4ra_prepare_data()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_prepare_data.md)
