[![R build
status](https://github.com/atfutures/m4ra/workflows/R-CMD-check/badge.svg)](https://github.com/atfutures/m4ra/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ATFutures/m4ra/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ATFutures/m4ra)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)

# m4ra: Many-to-Many Multi-Modal Routing Aggregator

- Travel times using any combination of three transport modes: foot,
  bicycle, and public transport.
- Ratios of multi-modal times to equivalent travel times in private
  automobile, including local infrastructure-specific estimates of time
  delays due to parking.

## Data Needs

1.  A representation of the local street network, in [`osmdata_sc`
    format](https://docs.ropensci.org/osmdata/reference/osmdata_sc.html),
    potentially including elevation data to accommodate effects of
    incline on pedestrian and bicycle travel times.
2.  A GTFS (General Transit Feed Specification)-format data file of
    public transport data for the study city or location.

## How?

The following parameters need to be specified:

1.  Name of city - used to name locally-cached files formed during
    pre-processing.
2.  Day of the week for which public transport is to be analysed.
3.  Starting time limits defining the public transport component of the
    journey, from which the fastest single journey to every destination
    will be calculated.
4.  Initial and final modes of transport, as either pedestrian or
    bicycle.
5.  A selection of starting points from which to calculate either
    multi-modal travel times, or durations of those times to equivalent
    automobile times.

Travel times are always calculated to every destination point in a given
network, with a single query returning matrices of times, one row for
each origin point, and columns holding travel times to all possible
destination points.
