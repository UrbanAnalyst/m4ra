# m4ra_vertices

Extract vertices of graph, including spatial coordinates if included.
Modified version of equivalent dodgr function that uses different hash
comparisons. This version also does none of the checks implemented in
the dodgr version, and assumes graphs remain unmodified throughout.

## Usage

``` r
m4ra_vertices(graph, city)
```

## Arguments

- graph:

  A flat table of graph edges. Must contain columns labelled `from` and
  `to`, or `start` and `stop`. May also contain similarly labelled
  columns of spatial coordinates (for example `from_x` or `stop_lon`).

- city:

  Name of city; used to name cached network files.

## Value

A `data.frame` of vertices with unique numbers (`n`).

## Note

Values of `n` are 0-indexed

## Examples

``` r
graph <- dodgr::weight_streetnet (dodgr::hampi)
v <- m4ra_vertices (graph, "hampi")
```
