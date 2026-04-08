# Calculate network distances to nearest 'n' points.

Calculate network distances to nearest 'n' points.

## Usage

``` r
m4ra_dists_n_pts(graph, from = NULL, to = NULL, npts = 3L)
```

## Arguments

- graph:

  A `dodgr` network returned from the weight_streetnet function using a
  network obtained with the osmdata `osmdata_sc` function.

- from:

  Vector of OpenStreetMap ID values from which route distances are to be
  calculated.

- to:

  Vector of OpenStreetMap ID values to which nearest 'npts' distances
  are to be calculated.

- npts:

  Number of points to which distances are to be calculated.

## See also

Other main:
[`m4ra_gtfs_traveltimes()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_gtfs_traveltimes.md),
[`m4ra_times_multi_mode()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_multi_mode.md),
[`m4ra_times_single_mode()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_single_mode.md)
