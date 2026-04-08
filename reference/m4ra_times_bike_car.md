# Calculate relative times from a specified point by bicycle versus private automobile, along with (optionally) corresponding walking distances.

This city expects weighted networks to have been generated with the
[m4ra_batch_weight_networks](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_batch_weight_networks.md)
function, and for corresponding networks to exist in the `m4ra` cache
directory for the specified city.

## Usage

``` r
m4ra_times_bike_car(city = NULL, from = NULL, walk_dists = TRUE)
```

## Arguments

- city:

  City for which values are to be calculated

- from:

  Vector of OSM ID values of vertices from which ratios are to be
  calculated. Typically obtained by loading one weighted network, and
  sampling or extracting vertices from the function `m4ra_vertices`.

- walk_dists:

  If `TRUE`, also calculate equivalent walking distances.

## Value

A `data.frame` of destination vertices, including Open Street Map ID
values, and longitude and latitude values, and four variables:

- 'car_t' Times in seconds to travel with automobile.

- 'bike_t' Times in seconds to travel by bicycles.

- 'walk_d' Equivalent walking distance in kilometres.

- 'ratio' Ratio of bicycle to automobile travel times.

## See also

Other analyses:
[`m4ra_bike_car_ratio_areas()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_bike_car_ratio_areas.md),
[`m4ra_times_mm_car()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_mm_car.md)

## Examples

``` r
if (FALSE) { # \dontrun{
city <- "<city_name>"
net <- m4ra_load_cached_network (city = city, mode = "foot")
v <- m4ra_vertices (net, "<city_name>")
from <- sample (v$id, size = 10)
dat <- m4ra_bike_car_ratios (city = city, from = from)
} # }
```
