# Return a unique hash for the input network.

This can be used to identify cached versions of a given network, in
particular to determine whether or not weighted versions of a given
network have been previously cached.

## Usage

``` r
m4ra_network_hash(graph)
```

## Arguments

- graph:

  A silicate, "SC", format object containing network data used to
  generate weighted street networks.

## Value

Single character value with unique hash of given network.

## See also

Other cache:
[`m4ra_batch_weight_networks()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_batch_weight_networks.md),
[`m4ra_cache_network()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_cache_network.md),
[`m4ra_load_cached_network()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_load_cached_network.md),
[`m4ra_weight_networks()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_weight_networks.md)
