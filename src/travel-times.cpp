
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
        Rcpp::IntegerMatrix t_gtfs_to_net)
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
        Rcpp::checkUserInterrupt ();

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

        Rcpp::checkUserInterrupt ();

        // Then minimal times from those stops to all other network points:
        for (size_t j = 0; j < n_gtfs; j++)
        {
            if (times_to_gtfs_stops (i, j) == INFINITE_INT)
            {
                continue;
            }

            for (size_t k = 0; k < n_verts; k++)
            {
                if (t_gtfs_to_net (j, k) == NA_INTEGER)
                {
                    continue;
                }

                const int t_to_net = times_to_gtfs_stops (i, j) +
                    t_gtfs_to_net (j, k);
                if (t_to_net < result (i, k))
                {
                    result (i, k) = t_to_net;
                }
            }
        }
    }

    for (int i = 0; i < n_from; i++)
    {
        for (size_t k = 0; k < n_verts; k++)
        {
            if (result (i, k) == INFINITE_INT)
            {
                result (i, k) = NA_INTEGER;
            }
        }
    }

    return result;
}
