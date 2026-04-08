# Load cached file for one city and mode

Load cached file for one city and mode

## Usage

``` r
m4ra_load_cached_network(
  city = NULL,
  mode = "foot",
  contracted = TRUE,
  filename = NULL
)
```

## Arguments

- city:

  City for which file is to be loaded.

- mode:

  One of "foot", "bicycle", or "motorcar".

- contracted:

  If `TRUE`, load the contracted version of the graph; otherwise load
  the full graph.

- filename:

  Optional name of specific file to load.

## Value

Previously cached, weighted streetnet for specified city and mode.

## See also

Other cache:
[`m4ra_batch_weight_networks()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_batch_weight_networks.md),
[`m4ra_cache_network()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_cache_network.md),
[`m4ra_network_hash()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_network_hash.md),
[`m4ra_weight_networks()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_weight_networks.md)
