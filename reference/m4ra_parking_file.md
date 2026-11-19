# Path to the cached file with parking data for a city.

This is the file of the results of
[m4ra_parking](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_parking.md),
which are used to add parking times to travel times by automobile.

## Usage

``` r
m4ra_parking_file(city_name, mode = "motorcar")
```

## Arguments

- city_name:

  Name of city used to name cached files.

- mode:

  Mode of transport of the network used to calculate parking data
  (should generally be "motorcar").

## Value

Path to the cached file, or `character (0)` if there is none. An error
is thrown if there is more than one file for the cached network.

## See also

Other cache:
[`m4ra_batch_weight_networks()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_batch_weight_networks.md),
[`m4ra_cache_network()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_cache_network.md),
[`m4ra_load_cached_network()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_load_cached_network.md),
[`m4ra_network_hash()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_network_hash.md),
[`m4ra_weight_networks()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_weight_networks.md)
