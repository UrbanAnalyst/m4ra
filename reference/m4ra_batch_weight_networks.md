# Cache a directory full of street networks for routing queries

This function runs of a directory which contain a number of silicate or
`sc`-formatted street networks, generated with the `dodgr_streetnet_sc`
function of the dodgr package. The function uses a default cache
location specified by
[`rappdirs::user_cache_dir()`](https://rappdirs.r-lib.org/reference/user_cache_dir.html).
This location can be over-ridden by specifying a local environment
variable, "M4RA_CACHE_DIR".

## Usage

``` r
m4ra_batch_weight_networks(net_dir, remove_these = NULL)
```

## Arguments

- net_dir:

  Path of local directory containing 'sc'-format street networks.

- remove_these:

  Names of any 'sc'-format files which should not be converted into
  weighted network form.

## Value

A character vector of local locations of cached versions of the
variously weighted network representations used in the various routing
functions.

## See also

Other cache:
[`m4ra_cache_network()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_cache_network.md),
[`m4ra_load_cached_network()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_load_cached_network.md),
[`m4ra_network_hash()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_network_hash.md),
[`m4ra_weight_networks()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_weight_networks.md)
