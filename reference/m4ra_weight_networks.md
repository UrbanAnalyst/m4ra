# Cache weighted networks for routing query

Uses a default cache location specified by
[`rappdirs::user_cache_dir()`](https://rappdirs.r-lib.org/reference/user_cache_dir.html).
This location can be over-ridden by specifying a local environment
variable, "M4RA_CACHE_DIR". The "city" parameter is only used as a
prefix for the cached networks.

## Usage

``` r
m4ra_weight_networks(net, city, quiet = TRUE)
```

## Arguments

- net:

  A silicate, "SC", format object containing network data used to
  generate weighted street networks.

- city:

  Name of city; only used to name cached network files.

- quiet:

  If `FALSE`, display progress information on screen.

## Value

A character vector of local locations of cached versions of the
variously weighted network representations used in the various routing
functions.

## See also

Other cache:
[`m4ra_batch_weight_networks()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_batch_weight_networks.md),
[`m4ra_cache_network()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_cache_network.md),
[`m4ra_load_cached_network()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_load_cached_network.md),
[`m4ra_network_hash()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_network_hash.md)
