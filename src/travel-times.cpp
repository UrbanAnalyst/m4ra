
#include "travel-times.h"

//' rcpp_scan_times
//'
//' This takes three matrices as inputs:
//' `net_to_gtfs` quantifying travel times from a selection of points to every
//' GTFS stop;
//' `gtfs_to_gtfs' holding the full GTFS travel time matrix; and
//' `gtfs_to_net` holding times back out from all GTFS stops to every network
//' point.
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::IntegerMatrix rcpp_net_gtfs_travel_times (Rcpp::IntegerMatrix t_net_to_gtfs,
        Rcpp::IntegerMatrix t_gtfs_to_gtfs,
        Rcpp::IntegerMatrix t_gtfs_to_net,
        const int n_closest = 10L)
{

    const int n_from = t_net_to_gtfs.nrow ();
    const int n_gtfs = t_net_to_gtfs.ncol ();
    if ((n_gtfs != t_gtfs_to_gtfs.nrow () || n_gtfs != t_gtfs_to_net.nrow ()))
    {
        Rcpp::stop ("network and gtfs matrices have incompatible dimensions.");
    }
    const int n_verts = t_gtfs_to_net.ncol ();

    Rcpp::IntegerMatrix result (n_from, n_verts);
    std::fill (result.begin (), result.end (), INFINITE_INT);

    // Construct times to all terminal GTFS stops as 
    for (size_t i = 0; i < n_from; i++)
    {
        // Minimal travel times to all GTFS stops:
        Rcpp::IntegerMatrix times_to_gtfs_stops (n_from, n_gtfs);
        std::fill (times_to_gtfs_stops.begin (), times_to_gtfs_stops.end (), INFINITE_INT);

        for (size_t j = 0; j < n_gtfs; j++)
        {
            if (t_net_to_gtfs (i, j) == NA_INTEGER)
            {
                continue;
            }

            for (size_t k = 0; k < n_gtfs; k++)
            {
                if (t_gtfs_to_gtfs (j, k) == NA_INTEGER)
                {
                    continue;
                }

                const int t_to_gtfs_k = t_net_to_gtfs (i, j) + t_gtfs_to_gtfs (j, k);
                if (t_to_gtfs_k < times_to_gtfs_stops (i, k))
                {
                    times_to_gtfs_stops (i, k) = t_to_gtfs_k;
                }
            }
        }

        std::vector <int> closest_gtfs = get_closest_gtfs_stns (times_to_gtfs_stops, i, n_closest);

        // Then minimal times from those 'n_closest' stops to all other network points:
        for (size_t j = 0; j < n_closest; j++)
        {
            if (times_to_gtfs_stops (i, closest_gtfs [j]) == INFINITE_INT)
            {
                continue;
            }

            for (size_t k = 0; k < n_verts; k++)
            {
                if (t_gtfs_to_net (closest_gtfs [j], k) == NA_INTEGER)
                {
                    continue;
                }

                const int t_to_net = times_to_gtfs_stops (i, closest_gtfs [j]) +
                    t_gtfs_to_net (closest_gtfs [j], k);
                if (t_to_net < result (i, k))
                {
                    result (i, k) = t_to_net;
                }
            }
        }
        Rcpp::checkUserInterrupt ();
    }

    Rcpp::checkUserInterrupt ();
    for (int i = 0; i < n_from; i++)
    {
        for (size_t j = 0; j < n_verts; j++)
        {
            if (result (i, j) == INFINITE_INT)
            {
                result (i, j) = NA_INTEGER;
            }
        }
    }

    return result;
}

std::vector <int> get_closest_gtfs_stns (Rcpp::IntegerMatrix times_to_gtfs_stops,
        const int i, const int n_closest)
{

    const int n_gtfs = times_to_gtfs_stops.ncol ();

    std::map <int, int> gtfs_stn_time_map;
    for (size_t j = 0; j < n_gtfs; j++)
    {
        if (times_to_gtfs_stops (i, j) == INFINITE_INT)
        {
            continue;
        }

        gtfs_stn_time_map.emplace (times_to_gtfs_stops (i, j), j);
    }

    std::vector <int> closest_gtfs (n_closest);
    auto g_ptr = gtfs_stn_time_map.begin ();
    for (size_t j = 0; j < n_closest; j++)
    {
        closest_gtfs [j] = g_ptr->second;
        std::advance (g_ptr, 1L);
    }
    gtfs_stn_time_map.clear ();

    return closest_gtfs;
}
