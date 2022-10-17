#' Calculate times from a specified point by bicycle and automobile, along with
#' corresponding walking distances.
#'
#' This city expects weighted networks to have been generated with the 
#' \link{m4ra_batch_weight_networks} function, and for corresponding networks to
#' exist in the `m4ra` cache directory for the specified city.
#'
#' @param city City for which values are to be calculated
#' @param from Vector of OSM ID values of vertices from which ratios are to be
#' calculated. Typically obtained by loading one weighted network, and
#' sampling or extracting vertices from the \pkg{dodgr} function
#' `dodgr_vertices`.
#' @param ratio_limits Limits of ratios of bicycle to automobile times for which
#' areas are to be calculated.
#' @return A `data.frame` of destination vertices, including Open Street Map ID
#' values, and longitude and latitude values, and four variables:
#' \itemize{
#' \item 'car_t' Times in seconds to travel with automobile.
#' \item 'bike_t' Times in seconds to travel by bicycles.
#' \item 'walk_d' Equivalent walking distance in kilometres.
#' \item 'ratio' Ratio of bicycle to automobile travel times.
#' }
#'
#' @examples
#' \dontrun{
#' city <- "<city_name>"
#' net <- m4ra_load_cached_network (city = city, mode = "foot")
#' v <- dodgr::dodgr_vertices (net)
#' from <- sample (v$id, size = 10)
#' dat <- m4ra_bike_car_ratios (city = city, from = from)
#' }
#' @family analyses
#' @export

m4ra_bike_car_times <- function (city = NULL, from = NULL) {

    requireNamespace ("dplyr")

    graph_f <- m4ra_load_cached_network (city, mode = "foot")
    v_f <- dodgr::dodgr_vertices (graph_f)
    graph_b <- m4ra_load_cached_network (city, mode = "bicycle")
    v_b <- dodgr::dodgr_vertices (graph_b)
    graph_c <- m4ra_load_cached_network (city, mode = "motorcar")
    v_c <- dodgr::dodgr_vertices (graph_c)

    v <- rbind (v_f, v_b, v_c)
    ids <- unique (v$id)
    v <- v [match (ids, v$id), ]

    car_times <- m4ra_times_single_mode (graph_c, from = from)
    car_times <- data.frame (
        id = colnames (car_times),
        car_t = car_times [1, ]
    )

    bike_times <- m4ra_times_single_mode (graph_b, from = from)
    bike_times <- data.frame (
        id = colnames (bike_times),
        bike_t = bike_times [1, ]
    )

    walk_dists <- dodgr::dodgr_distances (graph_f, from = from)
    walk_dists <- data.frame (
        id = colnames (walk_dists),
        walk_d = walk_dists [1, ]
    )

    dat <- dplyr::left_join (car_times, bike_times, by = "id") |>
        dplyr::left_join (walk_dists, by = "id") |>
        dplyr::mutate (ratio = bike_t / car_t) |>
        dplyr::mutate (walk_d = walk_d / 1000) |>
        dplyr::filter (!is.na (ratio) & !is.na (walk_d)) |>
        dplyr::left_join (v, by = "id") |>
        dplyr::select (-c ("component", "n"))

    return (dat)
}
