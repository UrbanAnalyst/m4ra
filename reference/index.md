# Package index

## Routing functions

- [`m4ra_dists_n_pts()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_dists_n_pts.md)
  : Calculate network distances to nearest 'n' points.
- [`m4ra_gtfs_traveltimes()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_gtfs_traveltimes.md)
  : Construct a travel time matrix between all pairs of stops in a
  'GTFS' feed.
- [`m4ra_times_multi_mode()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_multi_mode.md)
  : Calculate matrix of pair-wise travel times between points using
  multiple modes of transport.
- [`m4ra_times_single_mode()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_single_mode.md)
  : Calculate matrix of pair-wise travel times between points using a
  single mode of transport.

## Analysis and meta-routing functions

- [`m4ra_bike_car_ratio_areas()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_bike_car_ratio_areas.md)
  : Convert bicycle and automobile times into equivalent areas for a
  specified range of ratios of the two travel times.
- [`m4ra_times_bike_car()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_bike_car.md)
  : Calculate relative times from a specified point by bicycle versus
  private automobile, along with (optionally) corresponding walking
  distances.
- [`m4ra_times_mm_car()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_mm_car.md)
  : Calculate relative times from a specified point by multi-modal
  transport versus private automobile, along with (optionally)
  corresponding walking distances.

## Data preparation

- [`m4ra_parking()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_parking.md)
  : Get data on parking availability and building volumes to estimate
  time penalties for automobile travel.
- [`m4ra_prepare_data()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_prepare_data.md)
  : Prepare network and GTFS components for 'm4ra' queries.
- [`m4ra_times_to_gtfs_stops()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_to_gtfs_stops.md)
  : Construct a travel time matrix to or from all stops in a 'GTFS' feed
  from or to to all points in a street network.

## Save, Load, and Caching

- [`m4ra_batch_weight_networks()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_batch_weight_networks.md)
  : Cache a directory full of street networks for routing queries
- [`m4ra_cache_network()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_cache_network.md)
  : Save a weighted street network in both full and contracted forms to
  local cache.
- [`m4ra_load_cached_network()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_load_cached_network.md)
  : Load cached file for one city and mode
- [`m4ra_network_hash()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_network_hash.md)
  : Return a unique hash for the input network.
- [`m4ra_weight_networks()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_weight_networks.md)
  : Cache weighted networks for routing query

## Miscellaneous functions

- [`m4ra_vertices()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_vertices.md)
  : m4ra_vertices

## Package Data

- [`m4ra_hampi`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_hampi.md)
  : m4ra_hampi
