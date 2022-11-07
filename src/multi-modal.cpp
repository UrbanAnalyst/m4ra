
#include "multi-modal.h"

//' rcpp_add_net_to_gtfs
//'
//' Add times from selected start points to all GTFS stations to total GTFS '
//' travel time matrix to generate fastest travel times to all GTFS end points.
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_add_net_to_gtfs (Rcpp::NumericMatrix net_times,
        Rcpp::NumericMatrix gtfs_times, Rcpp::List gtfs_to_net_index,
        Rcpp::List gtfs_to_net_dist, const int nverts)
{
    const int nfrom = net_times.nrow ();
    const int n_gtfs = net_times.ncol ();

    if (gtfs_times.nrow () != gtfs_times.ncol () &&
            gtfs_times.nrow () != n_gtfs)
    {
        Rcpp::stop ("GTFS and travel time matrices are not compatible");
    }

    Rcpp::NumericMatrix times_to_end_stops (nfrom, n_gtfs);
    std::fill (times_to_end_stops.begin (), times_to_end_stops.end (), INFINITE_DBL);

    for (int i = 0; i < nfrom; i++) {

        for (int j = 0; j < n_gtfs; j++) {

            const double time_i_to_j = net_times (i, j);

            for (int k = 0; k < n_gtfs; k++) {

                const double time_j_to_k = gtfs_times (j, k);
                const double time_i_to_k = std::min (net_times (i, k), time_i_to_j + time_j_to_k);

                if (time_i_to_k < times_to_end_stops (i, k))
                {
                    times_to_end_stops (i, k) = time_i_to_k;
                }
            }
        }
    }

    // Those are then the fastest times to each terminal GTFS stop. Then use the
    // lists of closest stops to each network point to calculate final fastest
    // times to each terminal network point.

    Rcpp::NumericMatrix res (nfrom, nverts);
    std::fill (res.begin (), res.end (), INFINITE_DBL);

    for (int i = 0; i < nfrom; i++)
    {
        for (int j = 0; j < gtfs_to_net_index.size (); j++)
        {
            Rcpp::IntegerVector index_j = gtfs_to_net_index (j);
            Rcpp::NumericVector d_j = gtfs_to_net_dist (j);

            for (int k = 0; k < index_j.size (); k++)
            {
                const double time_i_to_k = times_to_end_stops (i, j) + d_j (k);

                if (time_i_to_k < res (i, index_j [k]))
                {
                    res (i, index_j [k]) = time_i_to_k;
                }
            }
        }
    }

    return res;
}
