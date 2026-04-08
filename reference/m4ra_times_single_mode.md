# Calculate matrix of pair-wise travel times between points using a single mode of transport.

Calculate matrix of pair-wise travel times between points using a single
mode of transport.

## Usage

``` r
m4ra_times_single_mode(graph, from = NULL, to = NULL, path = NULL)
```

## Arguments

- graph:

  A `dodgr` network returned from the weight_streetnet function using a
  network obtained with the osmdata `osmdata_sc` function.

- from:

  Vector or matrix of points **from** which route distances are to be
  calculated (see Notes)

- to:

  Vector or matrix of points **to** which route distances are to be
  calculated (see Notes)

- path:

  If specified, save individual travel time vectors for each 'from'
  point at that local directory.

  `from` and `to` values can be either two-column matrices or equivalent
  of longitude and latitude coordinates, or else single columns
  precisely matching node numbers or names given in `graph$from` or
  `graph$to`. If `to` is `NULL`, pairwise distances are calculated
  between all points specified in `from`. If both `from` and `to` are
  `NULL`, pairwise distances are calculated between all nodes in
  `graph`.

## See also

Other main:
[`m4ra_dists_n_pts()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_dists_n_pts.md),
[`m4ra_gtfs_traveltimes()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_gtfs_traveltimes.md),
[`m4ra_times_multi_mode()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_multi_mode.md)

## Examples

``` r
net <- dodgr::weight_streetnet (m4ra_hampi, wt_profile = "foot")
#> Loading required namespace: dplyr
traveltimes <- m4ra_times_single_mode (net)
```
