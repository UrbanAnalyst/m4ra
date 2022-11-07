
#include "multi-modal.h"

//' rcpp_add_net_to_gtfs
//'
//' Add times from selected start points to all GTFS stations to total GTFS '
//travel time matrix to generate fastest travel times to all GTFS end points.
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_add_net_to_gtfs (Rcpp::NumericMatrix net_times,
        Rcpp::NumericMatrix gtfs_times)
{
    const int nfrom = net_times.nrow ();
    const int n_gtfs = net_times.ncol ();
    if (gtfs_times.nrow () != gtfs_times.ncol () &&
            gtfs_times.nrow () != n_gtfs)
    {
        Rcpp::stop ("GTFS and travel time matrices are not compatible");
    }

    Rcpp::NumericMatrix res (nfrom, n_gtfs);
    std::fill (res.begin (), res.end (), INFINITE_DBL);

    for (int i = 0; i < nfrom; i++) {

        std::vector <double> end_times (n_gtfs, INFINITE_DBL);

        for (int j = 0; j < n_gtfs; j++) {
            const double time_i_to_j = net_times (i, j);
            for (int k = 0; k < n_gtfs; k++) {
                const double time_j_to_k = gtfs_times (j, k);
                const double time_i_to_k = time_i_to_j + time_j_to_k;
                if (time_i_to_k < res (i, k))
                {
                    res (i, k) = time_i_to_k;
                }
            }
        }
    }

    return res;
}
