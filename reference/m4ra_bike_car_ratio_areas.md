# Convert bicycle and automobile times into equivalent areas for a specified range of ratios of the two travel times.

Convert bicycle and automobile times into equivalent areas for a
specified range of ratios of the two travel times.

## Usage

``` r
m4ra_bike_car_ratio_areas(bike_car_dat, ratio_lims = 1 + 0:10/10)
```

## Arguments

- bike_car_dat:

  Result of
  [m4ra_times_bike_car](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_bike_car.md)
  function for a specified vector of `from` points.

- ratio_lims:

  Vector of ratio limits used to calculate areas within which ratio of
  bicycle to automobile times is less than specified limits.

## Value

A `data.frame` with one row for each `from` point used to calculate
[m4ra_times_bike_car](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_bike_car.md),
and columns including the OSM "id" value and corresponding coordinates
("x" and "y"), followed by a series of columns, one for each specified
value of `ratio_lims`, containing the areas in square kilometres over
which bicycle travel times are within the specified ratio of car travel
times.

## See also

Other analyses:
[`m4ra_times_bike_car()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_bike_car.md),
[`m4ra_times_mm_car()`](https://UrbanAnalyst.github.io/m4ra/reference/m4ra_times_mm_car.md)
